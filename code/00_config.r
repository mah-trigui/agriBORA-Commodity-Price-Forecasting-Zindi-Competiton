# ============================================================
# 00_config.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Global configuration — libraries, paths, constants,
#           helper functions, and competition metric.
# ============================================================

# ============================================================
# 0. LIBRARIES
# ============================================================
library(tidyverse)
library(lubridate)
library(zoo)
library(imputeTS)
library(forecast)
library(prophet)
library(xgboost)
library(lightgbm)
library(glmnet)
library(caret)

# ============================================================
# 1. PATHS
# ============================================================
DATA_DIR <- "C:/Users/mtrigui2/Desktop/Z1"
KAMIS_PATH <- file.path(DATA_DIR, "kamis_maize_prices.csv")
AGRI_PATH <- file.path(DATA_DIR, "agribora_maize_prices.csv")
AGRI_SUP_PATH <- file.path(DATA_DIR, "agriBORA_maize_prices_weeks_46_to_51.csv")
AGRI_FINAL_PATH <- file.path(DATA_DIR, "agriBORA_Final_Weeks_maize_price.csv")
SAMPLE_SUB_PATH <- file.path(DATA_DIR, "SampleSubmission.csv")
SUBMIT_DIR <- DATA_DIR

# ============================================================
# 2. COMPETITION CONSTANTS
# ============================================================
COUNTIES <- c("Kiambu", "Kirinyaga", "Mombasa", "Nairobi", "Uasin-Gishu")

# Forecast periods (rolling 2-week-ahead throughout competition)
# Final required forecasts: Week 52 (2025) and Week 1 (2026, labelled "Week_1")
FORECAST_WEEKS <- list(
    round1 = c(48, 49), # Nov 24 – Dec 6  2025
    round2 = c(49, 50), # Dec 1  – Dec 13 2025
    round3 = c(50, 51), # Dec 8  – Dec 20 2025
    round4 = c(51, 52), # Dec 15 – Dec 27 2025
    round5 = c(52, 1), # Dec 22 – Jan  3 2026   (Week 1 = Week 53 for 2025)
    round6 = c(1, 2) # Dec 29 – Jan 10 2026
)

SEED <- 1618
set.seed(SEED)

# ============================================================
# 3. COMPETITION METRIC
#    Score = 0.5 * MAE + 0.5 * RMSE
# ============================================================
competition_score <- function(actual, predicted) {
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    list(score = 0.5 * mae + 0.5 * rmse, mae = mae, rmse = rmse)
}

# ============================================================
# 4. HELPER FUNCTIONS
# ============================================================

# Snap any date to the nearest Monday  (KAMIS dates are not always Monday)
snap_to_monday <- function(dates) {
    dates <- as.Date(dates)
    dow <- wday(dates, week_start = 1) # 1 = Monday
    prev_m <- as.integer(dow) - 1
    next_m <- ifelse(dow == 1L, 0L, 8L - as.integer(dow))
    ifelse(prev_m <= next_m,
        as.character(dates - prev_m),
        as.character(dates + next_m)
    ) |> as.Date()
}

# ISO week number of a date
iso_week <- function(d) isoweek(as.Date(d))

# Convert a week-start date to the competition ID label
# e.g. 2025-12-22 → "Kiambu_Week_52"
make_id <- function(county, week_date) {
    wk <- iso_week(week_date)
    paste0(county, "_Week_", wk)
}

# LOCF gap-fill (last-observation-carried-forward)
locf_fill <- function(x) zoo::na.fill(x, "extend")

# Walk-forward cross-validation windows (chronological splits)
make_cv_windows <- function(data, n_windows = 5, horizon = 2,
                            min_train_weeks = 52) {
    n <- nrow(data)
    out <- vector("list", n_windows)
    for (i in seq_len(n_windows)) {
        train_end <- n - horizon * (n_windows - i + 1)
        if (train_end < min_train_weeks) next
        test_start <- train_end + 1
        test_end <- min(train_end + horizon, n)
        out[[i]] <- list(
            train = 1:train_end,
            test  = test_start:test_end
        )
    }
    Filter(Negate(is.null), out)
}

cat("✓ 00_config.r loaded\n")
