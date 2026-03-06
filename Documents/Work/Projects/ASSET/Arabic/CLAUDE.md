# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains psychometric analysis pipelines for the **ASSET Arabic assessment** (subject_no=25). The workflow processes student response data through Item Response Theory (IRT) to produce scaled scores and percentile mappings.

## Workflow

The pipeline runs in two stages:

**Stage 1 — Data Preparation (Python)**
Run `Y2026/arabic_data_preparation.ipynb` in Jupyter. It:
1. Connects to MySQL via config at `/Users/arvind/Documents/Work/Code/PyScripts/configs/adrds.ini`
2. Queries student responses for a given `test_edition_id` (currently `51`) and `productId` (`1`)
3. Builds per-level response pivot matrices and writes them to `Y2026/level_{1..9}_pivot.csv`
4. Writes `Y2026/final_merged_df.csv` (long-format: summary_assessment_id, qcode, result)

**Stage 2 — IRT Calculation (R)**
Run `Y2026/irt_calculation.R`. It:
1. Reads `level_1_pivot.csv` (update the filename for other levels)
2. Fits a 2PL IRT model using `ltm(response_matrix ~ z1, IRT.param = TRUE)`
3. Estimates student abilities using `mlebme(..., method = 'BL')`

## Key Constants

| Constant | Value | Meaning |
|---|---|---|
| `subjectno` | 25 | Arabic subject |
| `productId` | 1 | ASSET product |
| `test_edition_id` | 51 | Current year's test edition (update each year) |

## Running the Notebooks

```bash
# Launch Jupyter for data preparation
jupyter notebook Y2026/arabic_data_preparation.ipynb

# Form consistency check
jupyter notebook formcheck.ipynb
```

```r
# R dependencies
install.packages(c("RMySQL", "irtoys", "Matrix", "jsonlite", "tidyverse", "dplyr", "tidyr", "ltm"))

# Run IRT calculation (set working directory first)
Rscript Y2026/irt_calculation.R
```

## Database Configuration

MySQL credentials are stored in `/Users/arvind/Documents/Work/Code/PyScripts/configs/adrds.ini` under the `[mysql]` section (keys: `username`, `password`, `host`, `port`, `database`). The notebook connects to the production read replica on AWS RDS.

## Key Files

- `formcheck.ipynb` — Verifies that all papers within a class have identical question sets (flags class 7 discrepancies)
- `Y2026/arabic_data_preparation.ipynb` — Full data pull and pivot matrix generation
- `Y2026/irt_calculation.R` — 2PL IRT fitting and ability estimation
- `percentile_sql_statements.txt` — Generated SQL `INSERT` statements for `assetD_ss_to_percentiles_mapping` table (subject_no=25, classes 1–9)
- `arabic_questionParameters_consolidated.csv` — Consolidated IRT item parameters across levels
- `arabic_answer_key.csv` — Answer key reference

## Data Schema Notes

- `studentAssessmentSummary` (asset schema): one row per student attempt, holds `raw_score`
- `studentAssessmentDetails` (asset schema): one row per question per attempt (`qno`, `result` 0/1)
- `assetD_assessment` / `assetD_paperBank` (educatio_educat schema): links students to dynamic paper and `qcode_list`
- Pivot CSVs: rows = `summary_assessment_id`, columns = `qcode`, values = 0/1 response
