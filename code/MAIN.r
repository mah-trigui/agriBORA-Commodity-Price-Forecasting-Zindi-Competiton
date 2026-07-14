# ============================================================
# MAIN.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Full end-to-end pipeline orchestrator.
#           Run this file to reproduce all results from
#           raw data to final submission CSV.
# ============================================================

cat("╔══════════════════════════════════════════════════════╗\n")
cat("║     KENYA MAIZE PRICE FORECASTING  — PIPELINE       ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")

# Locate the pipeline directory
PIPELINE_DIR <- dirname(sys.frame(1)$ofile)
if (!nchar(PIPELINE_DIR)) PIPELINE_DIR <- getwd() # fallback (interactive)

source_step <- function(file, step_label) {
    cat(strrep("═", 58), "\n")
    cat("  STEP:", step_label, "\n")
    cat(strrep("═", 58), "\n")
    t0 <- proc.time()
    source(file.path(PIPELINE_DIR, file), echo = FALSE)
    elapsed <- (proc.time() - t0)["elapsed"]
    cat("  ↳ Completed in", round(elapsed, 1), "seconds\n\n")
}

# ── 00 Configuration ─────────────────────────────────────
source_step("00_config.r", "00 · Configuration & helpers")

# ── 01 Data Loading ──────────────────────────────────────
source_step("01_data_loading.r", "01 · Load raw data")

# ── 02 Data Cleaning ─────────────────────────────────────
source_step("02_data_cleaning.r", "02 · Clean & calibrate series")

# ── 03 Feature Engineering ───────────────────────────────
source_step("03_feature_engineering.r", "03 · Build feature matrices")

# ── 04 Train / Test Split ────────────────────────────────
source_step("04_build_train_test.r", "04 · Chronological split")

# ── 05 Modelling ─────────────────────────────────────────
source_step("05_models.r", "05 · Train models")

# ── 06 Evaluation ────────────────────────────────────────
source_step("06_evaluation.r", "06 · CV & leaderboard")

# ── 07 Submission ────────────────────────────────────────
source_step("07_submission.r", "07 · Generate submission")

# ── Done ─────────────────────────────────────────────────
cat("╔══════════════════════════════════════════════════════╗\n")
cat("║                  PIPELINE COMPLETE                  ║\n")
cat("╚══════════════════════════════════════════════════════╝\n\n")
cat("Submission saved to:", SUBMIT_DIR, "\n\n")
