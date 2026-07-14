# ============================================================
# 07_submission.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Generate rolling 2-week-ahead forecasts for all
#           5 counties, build the competition submission CSV.
#
# Submission ID format: {County}_Week_{iso_week}
#   e.g.  Kiambu_Week_52,  Uasin-Gishu_Week_1
#
# Both Target_RMSE and Target_MAE must hold the same value
# (the predicted price probability).
#
# Requires: county_features, county_features_clean,
#           county_models, leaderboard_df
# Outputs : submission_YYYY-MM-DD.csv  in SUBMIT_DIR
# ============================================================

cat("\n========== 07: SUBMISSION ==========\n\n")

# ============================================================
# 1. SELECT BEST MODEL PER COUNTY
#    Use the holdout leaderboard ranking; fall back to ensemble.
# ============================================================
cat("1. Selecting best model per county...\n\n")

get_best_model_name <- function(county_name) {
    if (!exists("leaderboard_df") || nrow(leaderboard_df) == 0) {
        return("ensemble")
    }
    sub <- leaderboard_df[leaderboard_df$county == county_name, ]
    if (nrow(sub) == 0) {
        return("ensemble")
    }
    sub$model[which.min(sub$score)]
}

best_models <- setNames(
    sapply(COUNTIES, get_best_model_name),
    COUNTIES
)
cat("Best models selected:\n")
for (cn in COUNTIES) cat(" ", cn, "→", best_models[cn], "\n")
cat("\n")

# ============================================================
# 2. RETRAIN ON FULL DATA  (train + holdout, up to today)
# ============================================================
cat("2. Retraining best models on full dataset...\n\n")

full_models <- list()

for (cn in COUNTIES) {
    full_df <- county_features[[cn]] %>% arrange(week)
    feats <- county_features_clean[[cn]]$core
    mdl_nm <- best_models[cn]

    cat("  Retraining", cn, "(", mdl_nm, ")... ")

    tryCatch(
        {
            mdl <- if (grepl("lgbm", mdl_nm)) {
                feat_list <- if (grepl("all", mdl_nm)) {
                    county_features_clean[[cn]]$all
                } else {
                    county_features_clean[[cn]]$core
                }
                train_lgbm(full_df, feat_list, nrounds = 600)
            } else if (grepl("xgb", mdl_nm)) {
                train_xgb(full_df, feats, nrounds = 600)
            } else if (mdl_nm == "arima") {
                train_arima(full_df)
            } else if (mdl_nm == "ets") {
                train_ets(full_df)
            } else if (mdl_nm == "prophet") {
                train_prophet(full_df)
            } else {
                # ensemble: keep LightGBM as proxy
                train_lgbm(full_df, feats, nrounds = 600)
            }
            full_models[[cn]] <- mdl
            cat("✓\n")
        },
        error = function(e) {
            cat("✗", e$message, "\n")
            # Fallback: use previously trained model
            full_models[[cn]] <<- county_models[[cn]]$lgbm_core
        }
    )
}

# ============================================================
# 3. ITERATIVE 2-WEEK-AHEAD ROLLING FORECAST
#    Each call extends the history by 1 week before forecasting
#    the next.  This mirrors the actual competition protocol.
# ============================================================
cat("\n3. Generating rolling 2-week-ahead forecasts...\n\n")

# Forecast dates we need (competition covers Nov 17 – Jan 10)
# The actual weeks to predict are determined by last data available.
# Here we forecast 2 weeks beyond the last known week.

make_future_row <- function(history, future_week) {
    # Build a 1-row tibble with lag/rolling features set from history
    last <- tail(history, 1)
    h <- history$price
    n <- length(h)

    lag_row <- tibble(
        week = future_week,
        month = month(future_week),
        price_lag1 = if (n >= 1) h[n] else NA,
        price_lag2 = if (n >= 2) h[n - 1] else NA,
        price_lag3 = if (n >= 3) h[n - 2] else NA,
        price_lag4 = if (n >= 4) h[n - 3] else NA,
        price_lag5 = if (n >= 5) h[n - 4] else NA,
        price_lag6 = if (n >= 6) h[n - 5] else NA,
        price_lag8 = if (n >= 8) h[n - 7] else NA,
        price_lag12 = if (n >= 12) h[n - 11] else NA,
        price_lag16 = if (n >= 16) h[n - 15] else NA,
        price_lag26 = if (n >= 26) h[n - 25] else NA,
        price_lag52 = if (n >= 52) h[n - 51] else NA,
        price_ma4 = if (n >= 4) mean(tail(h, 4)) else mean(h),
        price_ma8 = if (n >= 8) mean(tail(h, 8)) else mean(h),
        price_ma12 = if (n >= 12) mean(tail(h, 12)) else mean(h),
        price_ma26 = if (n >= 26) mean(tail(h, 26)) else mean(h),
        price_ma52 = if (n >= 52) mean(tail(h, 52)) else mean(h),
        price_vol4 = if (n >= 4) sd(tail(h, 4)) else 0,
        price_vol12 = if (n >= 12) sd(tail(h, 12)) else 0,
        price_chg1 = if (n >= 2) h[n] - h[n - 1] else 0,
        price_chg4 = if (n >= 5) h[n] - h[n - 4] else 0,
        price_pct4 = if (n >= 5 && h[n - 4] > 0) {
            (h[n] - h[n - 4]) / h[n - 4] * 100
        } else {
            0
        },
        price_momentum4_8 = if (n >= 8) {
            mean(tail(h, 4)) - mean(tail(h, 8))
        } else {
            0
        },
        price_momentum12_52 = if (n >= 52) {
            mean(tail(h, 12)) - mean(tail(h, 52))
        } else {
            0
        },
        price_accel = if (n >= 3) {
            (h[n] - h[n - 1]) - (h[n - 1] - h[n - 2])
        } else {
            0
        },
        price_trend_short = if (n >= 5) {
            mean(tail(h, 4)) - mean(tail(h[pmax(1, n - 5):n], 4))
        } else {
            0
        },
        price_trend_long = if (n >= 16) {
            mean(tail(h, 12)) - mean(h[pmax(1, n - 15):n])
        } else {
            0
        },
        dev_from_ma4 = if (n >= 4) {
            (h[n] - mean(tail(h, 4))) / mean(tail(h, 4)) * 100
        } else {
            0
        },
        dev_from_ma12 = if (n >= 12) {
            (h[n] - mean(tail(h, 12))) / mean(tail(h, 12)) * 100
        } else {
            0
        },
        kamis_lag1 = last$price_kamis,
        kamis_lag2 = last$price_kamis,
        kamis_lag4 = last$price_kamis,
        price_kamis_monthly = last$price_kamis_monthly,
        # Calendar
        week_of_year = isoweek(future_week),
        quarter = quarter(future_week),
        month_sin = sin(2 * pi * month(future_week) / 12),
        month_cos = cos(2 * pi * month(future_week) / 12),
        week_sin = sin(2 * pi * isoweek(future_week) / 52),
        week_cos = cos(2 * pi * isoweek(future_week) / 52),
        quarter_sin = sin(2 * pi * quarter(future_week) / 4),
        quarter_cos = cos(2 * pi * quarter(future_week) / 4),
        is_harvest_season = as.integer(month(future_week) %in% c(5, 6, 10)),
        is_lean_season = as.integer(month(future_week) %in% c(1, 2, 7, 8)),
        is_planting_season = as.integer(month(future_week) %in% c(3, 4, 9)),
        is_long_rains = as.integer(month(future_week) %in% c(3, 4, 5)),
        is_short_rains = as.integer(month(future_week) %in% c(10, 11, 12)),
        price_season = case_when(
            month(future_week) %in% c(4, 5, 6) ~ 1,
            month(future_week) %in% c(7, 8, 9) ~ 2,
            month(future_week) %in% c(10, 11, 12) ~ 3,
            TRUE ~ 4
        ),
        month_encoded = as.integer(case_when(
            month(future_week) %in% c(5, 9) ~ 1L,
            month(future_week) %in% c(4, 6, 8, 10) ~ 2L,
            month(future_week) %in% c(11, 12, 2, 3) ~ 3L,
            TRUE ~ 4L
        )),
        weeks_since_start = as.numeric(
            difftime(future_week, min(history$week), units = "weeks")
        )
    )

    lag_row
}

forecast_county_rolling <- function(cn, full_history, model_obj, feats,
                                    model_name, n_ahead = 2) {
    history <- full_history %>% arrange(week)
    last_week <- max(history$week)
    future_weeks <- seq(last_week + 7, by = 7, length.out = n_ahead)
    forecasts <- numeric(n_ahead)
    rolling_hist <- history

    for (i in seq_len(n_ahead)) {
        fw <- future_weeks[i]
        row <- make_future_row(rolling_hist, fw)

        pred <- tryCatch(
            {
                if (inherits(model_obj, "list") &&
                    model_obj$type %in% c("lgbm", "xgb")) {
                    p_raw <- if (model_obj$type == "lgbm") {
                        predict_lgbm(model_obj, row)
                    } else {
                        predict_xgb(model_obj, row)
                    }
                    pmax(p_raw, 0)
                } else if (grepl("arima|ets", model_name)) {
                    fc <- forecast::forecast(model_obj$model, h = i)
                    as.numeric(fc$mean)[i]
                } else if (model_name == "prophet") {
                    predict_prophet(model_obj, fw)
                } else {
                    tail(rolling_hist$price, 1) # last-value fallback
                }
            },
            error = function(e) tail(rolling_hist$price, 1)
        )

        forecasts[i] <- pred

        # Append predicted row to rolling history for next iteration
        new_row <- row
        new_row$price <- pred
        rolling_hist <- bind_rows(rolling_hist, new_row)
    }

    tibble(
        county        = cn,
        week          = future_weeks,
        week_iso      = isoweek(future_weeks),
        predicted     = forecasts
    )
}

all_forecasts <- list()

for (cn in COUNTIES) {
    full_df <- county_features[[cn]] %>% arrange(week)
    feats <- county_features_clean[[cn]]$core
    mdl_nm <- best_models[cn]
    mdl_obj <- full_models[[cn]]

    cat("  Forecasting", cn, "...\n")
    fc <- tryCatch(
        forecast_county_rolling(cn, full_df, mdl_obj, feats, mdl_nm, n_ahead = 2),
        error = function(e) {
            cat("    ✗ Fallback to last price:", e$message, "\n")
            last_p <- tail(full_df$price, 1)
            last_w <- max(full_df$week)
            tibble(
                county    = cn,
                week      = seq(last_w + 7, by = 7, length.out = 2),
                week_iso  = isoweek(seq(last_w + 7, by = 7, length.out = 2)),
                predicted = last_p
            )
        }
    )
    cat(
        "    Weeks:", paste(fc$week_iso, collapse = ", "),
        "| Prices:", paste(round(fc$predicted, 2), collapse = ", "), "\n"
    )
    all_forecasts[[cn]] <- fc
}

# ============================================================
# 4. BUILD SUBMISSION DATAFRAME
# ============================================================
cat("\n4. Building submission file...\n")

forecast_panel <- bind_rows(all_forecasts)

# Submission ID: County_Week_N  (Week 53 of 2025 → labelled "Week_1")
forecast_panel <- forecast_panel %>%
    mutate(
        ID          = paste0(county, "_Week_", week_iso),
        Target_RMSE = round(predicted, 4),
        Target_MAE  = round(predicted, 4)
    ) %>%
    select(ID, Target_RMSE, Target_MAE)

cat("\nSubmission preview:\n")
print(forecast_panel)
cat("\n")

# Validate
stopifnot(nrow(forecast_panel) == length(COUNTIES) * 2)
stopifnot(!any(is.na(forecast_panel$Target_RMSE)))
stopifnot(all(forecast_panel$Target_RMSE > 0))

# ============================================================
# 5. SAVE
# ============================================================
submit_file <- file.path(
    SUBMIT_DIR,
    paste0("submission_maize_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
)

write_csv(forecast_panel, submit_file)
cat("✓ Saved:", submit_file, "\n\n")

cat("✓ 07_submission.r complete\n\n")
