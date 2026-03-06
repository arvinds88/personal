library(RMySQL)
library(dplyr)
library(tidyr)
library(stringr)

# ── Config ────────────────────────────────────────────────────────────────────

parse_ini <- function(filepath) {
  lines <- readLines(filepath)
  config <- list()
  section <- NULL
  for (line in lines) {
    line <- trimws(line)
    if (grepl("^\\[", line)) {
      section <- gsub("[\\[\\]]", "", line)
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

# ── Connect ───────────────────────────────────────────────────────────────────

con <- dbConnect(
  RMySQL::MySQL(),
  username = username,
  password = password,
  host     = host,
  port     = port,
  dbname   = database
)
cat(sprintf("Connected to %s as %s\n", host, username))

# ── Step 1: Get schools with Arabic orders ────────────────────────────────────

schools_query <- "
SELECT DISTINCT o.schoolCode
FROM educatio_educat.assetD_orderMaster o
WHERE o.test_edition_id = 51
  AND o.productId = 1
  AND o.order_id IN (
      SELECT DISTINCT ob.order_id
      FROM educatio_educat.assetD_orderBreakup ob
      WHERE ob.ar > 0
  )
"

schools_df   <- dbGetQuery(con, schools_query)
school_codes <- na.omit(as.integer(schools_df$schoolCode))

if (length(school_codes) == 0) stop("No schools found.")
cat(sprintf("Found %d schools\n", length(school_codes)))

school_list <- paste(school_codes, collapse = ",")

# ── Step 2: Get mapped assessments ────────────────────────────────────────────

mapped_query <- sprintf("
SELECT
    a.id                          AS assetd_assessment_id,
    a.userID                      AS user_id,
    a.sessionId                   AS session_id,
    sa.assessmentid               AS summary_assessment_id,
    sa.raw_score,
    CAST(u.test_level AS UNSIGNED) AS level,
    pb.id                         AS paper_id,
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
", school_list)

mapped_df <- dbGetQuery(con, mapped_query)
cat(sprintf("Unique summary_assessment_id in mapped: %d\n", n_distinct(mapped_df$summary_assessment_id)))

# ── Step 3: Get per-question results ──────────────────────────────────────────

summary_ids      <- unique(as.integer(mapped_df$summary_assessment_id))
summary_ids_list <- paste(summary_ids, collapse = ",")

details_query <- sprintf("
SELECT
    sd.assessmentid AS summary_assessment_id,
    sd.qno,
    sd.result
FROM asset.studentAssessmentDetails sd
WHERE sd.assessmentid IN (%s)
", summary_ids_list)

details_df <- dbGetQuery(con, details_query)
cat(sprintf("Unique summary_assessment_id in details: %d\n", n_distinct(details_df$summary_assessment_id)))

dbDisconnect(con)

# ── Step 4: Merge details with level and paper_id ─────────────────────────────

merged_df <- details_df %>%
  left_join(
    mapped_df %>% select(summary_assessment_id, level, paper_id),
    by = "summary_assessment_id"
  ) %>%
  distinct()

# ── Step 5: Explode qcode_list → (paper_id, qno, qcode) ──────────────────────

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

# ── Step 6: Join to map qno → qcode on each student response ─────────────────

final_merged_df <- merged_df %>%
  left_join(exploded_qcodes_df, by = c("level", "paper_id", "qno"))

cat(sprintf("Unique result values: %s\n", paste(unique(final_merged_df$result), collapse = ", ")))

# ── Step 7: Write outputs ─────────────────────────────────────────────────────

output_dir <- "/Users/arvind/Documents/Work/Projects/ASSET/Arabic/Y2026/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(final_merged_df,
          file.path(output_dir, "final_merged_df.csv"),
          row.names = FALSE)
cat("Written final_merged_df.csv\n")

for (lvl in sort(unique(final_merged_df$level))) {
  pivot <- final_merged_df %>%
    filter(level == lvl) %>%
    select(summary_assessment_id, qcode, result) %>%
    pivot_wider(
      id_cols     = summary_assessment_id,
      names_from  = qcode,
      values_from = result
    )

  out_path <- file.path(output_dir, sprintf("level_%s_pivot.csv", lvl))
  write.csv(pivot, out_path, row.names = FALSE)
  cat(sprintf("Written level_%s_pivot.csv (%d students)\n", lvl, nrow(pivot)))
}
