library(RMySQL)
library(ltm)
library(irtoys)
library(dplyr)
library(tidyr)
library(stringr)

# ══════════════════════════════════════════════════════════════════════════════
# USER CONFIGURATION
# ══════════════════════════════════════════════════════════════════════════════

output_dir <- "/Users/arvind/Documents/Work/Projects/ASSET/Arabic/Y2026v4/"

# Qcodes to drop manually (raw qcode values, not prefixed with "X").
# Set to c() to skip manual dropping.
manual_drop_qcodes <- c(
  # e.g. "75617", "75618"
)

# Item parameter filter thresholds (applied after IRT calibration).
# Items outside these bounds are excluded from scoring.
b_lower <- -4    # difficulty lower bound (exclusive)
b_upper <-  4    # difficulty upper bound (exclusive)
a_min   <-  0    # discrimination minimum (exclusive; items with a <= a_min dropped)

# Model selection threshold.
# Levels with fewer students than this use the Rasch model (1PL) instead of 2PL.
# Level 9 always uses the Rasch model regardless of n_students.
rasch_threshold <- 200

# ══════════════════════════════════════════════════════════════════════════════

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ── Config ────────────────────────────────────────────────────────────────────

parse_ini <- function(filepath) {
  lines   <- readLines(filepath)
  config  <- list()
  section <- NULL
  for (line in lines) {
    line <- trimws(line)
    if (grepl("^\\[", line)) {
      section <- gsub("\\[|\\]", "", line)
      config[[section]] <- list()
    } else if (grepl("=", line) && !is.null(section)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      key   <- trimws(parts[1])
      value <- trimws(paste(parts[-1], collapse = "="))
      config[[section]][[key]] <- value
    }
  }
  config
}

cfg      <- parse_ini("/Users/arvind/Documents/Work/Code/PyScripts/configs/adrds.ini")
username <- cfg$mysql$username
password <- cfg$mysql$password
host     <- cfg$mysql$host
port     <- as.integer(cfg$mysql$port)
database <- cfg$mysql$database

# ── Step 1: Connect and pull data ─────────────────────────────────────────────

con <- dbConnect(
  RMySQL::MySQL(),
  username = username,
  password = password,
  host     = host,
  port     = port,
  dbname   = database
)
cat(sprintf("Connected to %s as %s\n", host, username))

schools_df   <- dbGetQuery(con, "
  SELECT DISTINCT o.schoolCode
  FROM educatio_educat.assetD_orderMaster o
  WHERE o.test_edition_id = 51
    AND o.productId = 1
    AND o.order_id IN (
        SELECT DISTINCT ob.order_id
        FROM educatio_educat.assetD_orderBreakup ob
        WHERE ob.ar > 0
    )
")
school_codes <- na.omit(as.integer(schools_df$schoolCode))
if (length(school_codes) == 0) stop("No schools found.")
cat(sprintf("Found %d schools\n", length(school_codes)))
school_list <- paste(school_codes, collapse = ",")

mapped_df <- dbGetQuery(con, sprintf("
  SELECT
      a.id                            AS assetd_assessment_id,
      a.userID                        AS user_id,
      a.sessionId                     AS session_id,
      sa.assessmentid                 AS summary_assessment_id,
      sa.raw_score,
      CAST(u.test_level AS UNSIGNED)  AS level,
      pb.id                           AS paper_id,
      pb.qcode_list
  FROM educatio_educat.assetD_assessment a
  JOIN educatio_educat.assetD_users u
      ON u.id = a.userID
  JOIN educatio_educat.assetD_orderMaster o
      ON o.order_id = u.order_id
  JOIN educatio_educat.assetD_paperBank pb
      ON pb.id = a.dynamicPaperId
      AND pb.productId = 1
      AND pb.subjectno = 25
  JOIN asset.studentAssessmentSummary sa
      ON sa.userid = a.userID
      AND sa.sessionid = a.sessionId
  WHERE o.test_edition_id = 51
      AND o.productId = 1
      AND a.subjectno = 25
      AND o.schoolCode IN (%s)
      AND CAST(u.test_level AS UNSIGNED) = CAST(pb.class AS UNSIGNED)
      AND pb.qcode_list IS NOT NULL
      AND TRIM(pb.qcode_list) <> ''
", school_list))
cat(sprintf("Unique assessments: %d\n", n_distinct(mapped_df$summary_assessment_id)))

summary_ids_list <- paste(unique(as.integer(mapped_df$summary_assessment_id)), collapse = ",")
details_df <- dbGetQuery(con, sprintf("
  SELECT
      sd.assessmentid AS summary_assessment_id,
      sd.qno,
      sd.result
  FROM asset.studentAssessmentDetails sd
  WHERE sd.assessmentid IN (%s)
", summary_ids_list))
cat(sprintf("Details rows fetched: %d\n", nrow(details_df)))

dbDisconnect(con)

# ── Step 2: Build final_merged_df ─────────────────────────────────────────────

merged_df <- details_df %>%
  left_join(
    mapped_df %>% select(summary_assessment_id, level, paper_id),
    by = "summary_assessment_id"
  ) %>%
  distinct()

exploded_qcodes_df <- mapped_df %>%
  distinct(level, paper_id, qcode_list) %>%
  separate_rows(qcode_list, sep = ",") %>%
  mutate(qcode = str_trim(qcode_list)) %>%
  filter(qcode != "") %>%
  select(level, paper_id, qcode) %>%
  group_by(paper_id) %>%
  mutate(qno = row_number()) %>%
  ungroup() %>%
  distinct()

final_merged_df <- merged_df %>%
  left_join(exploded_qcodes_df, by = c("level", "paper_id", "qno"))

write.csv(final_merged_df, file.path(output_dir, "final_merged_df.csv"), row.names = FALSE)
cat("Written final_merged_df.csv\n")

# ── Step 3: Build per-level pivot matrices and write CSVs ─────────────────────

levels     <- sort(unique(final_merged_df$level))
pivot_list <- list()

for (lvl in levels) {
  pivot <- final_merged_df %>%
    filter(level == lvl) %>%
    select(summary_assessment_id, qcode, result) %>%
    pivot_wider(
      id_cols     = summary_assessment_id,
      names_from  = qcode,
      values_from = result
    )
  pivot_list[[as.character(lvl)]] <- pivot
  write.csv(pivot, file.path(output_dir, sprintf("level_%s_pivot.csv", lvl)), row.names = FALSE)
  cat(sprintf("Level %s: %d students, %d items\n", lvl, nrow(pivot), ncol(pivot) - 1))
}

# ── Step 4: Fit IRT model per level, collect item parameters ──────────────────
#
# Model selection rules (in priority order):
#   1. Level 9          → Rasch (a = 1, c = 0, b from Rasch estimate)
#   2. n_students < rasch_threshold → Rasch (a = 1, c = 0, b from Rasch estimate)
#   3. Otherwise        → 2PL   (a, b from ltm, c = 0)

all_item_params <- list()

for (lvl in levels) {
  pivot           <- pivot_list[[as.character(lvl)]]
  response_matrix <- as.matrix(pivot[, -1])
  n_students      <- nrow(response_matrix)

  use_rasch <- (lvl == 9) || (n_students < rasch_threshold)
  model_label <- if (use_rasch) "Rasch" else "2PL"

  cat(sprintf("Fitting %s model for level %s (%d students, %d items)...\n",
              model_label, lvl, n_students, ncol(response_matrix)))

  if (use_rasch) {
    # Rasch model: discrimination fixed at 1, c = 0
    # coef() returns col1 = difficulty (b), col2 = discrimination (always 1)
    fit       <- rasch(response_matrix, IRT.param = TRUE)
    item_coef <- coef(fit)
    p_mat     <- cbind(1, item_coef[, 1], 0)
  } else {
    # 2PL model: discrimination free to vary, c = 0
    # coef() returns col1 = difficulty (b), col2 = discrimination (a)
    fit       <- ltm(response_matrix ~ z1, IRT.param = TRUE)
    item_coef <- coef(fit)
    p_mat     <- cbind(item_coef[, 2], item_coef[, 1], 0)
  }

  colnames(p_mat) <- c("a", "b", "c")

  params_df           <- as.data.frame(p_mat)
  params_df$qcode_x   <- paste0("X", rownames(p_mat))
  params_df$level     <- lvl
  params_df$irt_model <- model_label

  all_item_params[[as.character(lvl)]] <- params_df
  cat(sprintf("  Level %s: %d items calibrated (%s)\n", lvl, nrow(params_df), model_label))
}

# ── Step 4b: Drop items by parameter rules and manual list ────────────────────

manual_drop_qcodes_x <- paste0("X", as.character(manual_drop_qcodes))

dropped_log <- list()

for (lvl in levels) {
  params_df <- all_item_params[[as.character(lvl)]]
  n_before  <- nrow(params_df)

  drop_manual <- params_df$qcode_x %in% manual_drop_qcodes_x
  drop_b_low  <- params_df$b <= b_lower
  drop_b_high <- params_df$b >= b_upper
  drop_a_low  <- params_df$a <= a_min

  # Assign drop_reason on the full params_df before subsetting to avoid size mismatch
  params_df$drop_reason <- case_when(
    drop_manual ~ "manual",
    drop_b_low  ~ sprintf("b <= %g", b_lower),
    drop_b_high ~ sprintf("b >= %g", b_upper),
    drop_a_low  ~ sprintf("a <= %g", a_min),
    TRUE        ~ NA_character_
  )

  drop_any <- !is.na(params_df$drop_reason)

  if (any(drop_any)) {
    dropped <- params_df[drop_any, ]
    dropped_log[[as.character(lvl)]] <- dropped
    cat(sprintf("  Level %s: dropping %d / %d items — %s\n",
                lvl, nrow(dropped), n_before,
                paste(sprintf("%s (%s)", dropped$qcode_x, dropped$drop_reason), collapse = ", ")))
  }

  all_item_params[[as.character(lvl)]] <- params_df[!drop_any, ] %>% select(-drop_reason)
  cat(sprintf("  Level %s: %d items retained after filtering\n",
              lvl, nrow(all_item_params[[as.character(lvl)]])))
}

if (length(dropped_log) > 0) {
  dropped_df <- bind_rows(dropped_log)
  write.csv(dropped_df,
            file.path(output_dir, "arabic_dropped_items.csv"),
            row.names = FALSE)
  cat("Written arabic_dropped_items.csv\n")
} else {
  cat("No items dropped.\n")
}

combined_params <- bind_rows(all_item_params)
write.csv(combined_params,
          file.path(output_dir, "arabic_item_parameters_all_levels.csv"),
          row.names = FALSE)
cat("Written arabic_item_parameters_all_levels.csv\n")

# ── Step 5: Score each student ────────────────────────────────────────────────

id_meta <- mapped_df %>%
  select(summary_assessment_id, level, raw_score) %>%
  distinct()

all_ids <- unique(id_meta$summary_assessment_id)
n_total <- length(all_ids)
results  <- vector("list", n_total)

cat(sprintf("Scoring %d students...\n", n_total))

for (i in seq_along(all_ids)) {
  sid <- all_ids[i]

  meta      <- id_meta[id_meta$summary_assessment_id == sid, ][1, ]
  lvl       <- meta$level
  raw_score <- meta$raw_score

  student_long <- final_merged_df %>%
    filter(summary_assessment_id == sid, !is.na(qcode)) %>%
    select(qcode, result)

  n_items <- nrow(student_long)

  # Zero score: force scaled score to 200
  if (!is.na(raw_score) && raw_score == 0) {
    results[[i]] <- data.frame(
      summary_assessment_id = sid, level = lvl, raw_score = raw_score,
      n_items = n_items, est = NA_real_, sem = NA_real_, scaled_score = 200L
    )
    next
  }

  # Perfect score: force scaled score to 800
  if (!is.na(raw_score) && raw_score == n_items) {
    results[[i]] <- data.frame(
      summary_assessment_id = sid, level = lvl, raw_score = raw_score,
      n_items = n_items, est = NA_real_, sem = NA_real_, scaled_score = 800L
    )
    next
  }

  student_matrix <- matrix(
    student_long$result,
    nrow = 1,
    dimnames = list(NULL, paste0("X", student_long$qcode))
  )

  level_params <- all_item_params[[as.character(lvl)]]
  common_items <- intersect(colnames(student_matrix), level_params$qcode_x)

  if (length(common_items) == 0) {
    results[[i]] <- data.frame(
      summary_assessment_id = sid, level = lvl, raw_score = raw_score,
      n_items = n_items, est = NA_real_, sem = NA_real_, scaled_score = NA_integer_
    )
    next
  }

  student_matrix_filtered <- student_matrix[, common_items, drop = FALSE]

  param_rows            <- match(common_items, level_params$qcode_x)
  param_matrix_filtered <- as.matrix(level_params[param_rows, c("a", "b", "c")])
  rownames(param_matrix_filtered) <- common_items

  abilities <- tryCatch(
    mlebme(student_matrix_filtered, param_matrix_filtered, mu = 0, sigma = 1, method = "BL"),
    error = function(e) NULL
  )

  if (is.null(abilities)) {
    results[[i]] <- data.frame(
      summary_assessment_id = sid, level = lvl, raw_score = raw_score,
      n_items = n_items, est = NA_real_, sem = NA_real_, scaled_score = NA_integer_
    )
    next
  }

  est          <- abilities[1, 1]
  sem          <- abilities[1, 2]
  scaled_score <- max(200L, min(800L, as.integer(round(500 + 100 * est, 0))))

  results[[i]] <- data.frame(
    summary_assessment_id = sid, level = lvl, raw_score = raw_score,
    n_items = n_items, est = est, sem = sem, scaled_score = scaled_score
  )

  if (i %% 500 == 0) cat(sprintf("  Scored %d / %d\n", i, n_total))
}

# ── Step 6: Write output ──────────────────────────────────────────────────────

results_df <- bind_rows(results)

write.csv(results_df,
          file.path(output_dir, "arabic_student_scaled_scores.csv"),
          row.names = FALSE)
cat(sprintf("Done. Written arabic_student_scaled_scores.csv (%d rows)\n", nrow(results_df)))
