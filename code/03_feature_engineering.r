# ============================================================
# 03_feature_engineering.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Build the full feature matrix per county — lag
#           prices, rolling statistics, momentum, volatility,
#           cyclical calendar features, and seasonal dummies.
# Requires: county_data  (02_data_cleaning.r)
# Outputs : county_features — named list, one tibble per county
# ============================================================

cat("\n========== 03: FEATURE ENGINEERING ==========\n\n")

# ============================================================
# CORE FEATURE BUILDER  (applied per county)
# ============================================================
build_features <- function(df) {
    df <- df %>% arrange(week)

    df <- df %>%
        mutate(
            # ──────────────────────────────────────────────
            # 1. PRICE LAG FEATURES
            # ──────────────────────────────────────────────
            price_lag1 = lag(price, 1),
            price_lag2 = lag(price, 2),
            price_lag3 = lag(price, 3),
            price_lag4 = lag(price, 4),
            price_lag5 = lag(price, 5),
            price_lag6 = lag(price, 6),
            price_lag8 = lag(price, 8),
            price_lag12 = lag(price, 12),
            price_lag16 = lag(price, 16),
            price_lag26 = lag(price, 26),
            price_lag52 = lag(price, 52),

            # KAMIS lag features (external signal)
            kamis_lag1 = lag(price_kamis, 1),
            kamis_lag2 = lag(price_kamis, 2),
            kamis_lag4 = lag(price_kamis, 4),

            # ──────────────────────────────────────────────
            # 2. MOVING AVERAGES
            # ──────────────────────────────────────────────
            price_ma2 = zoo::rollmean(price, k = 2, fill = NA, align = "right"),
            price_ma3 = zoo::rollmean(price, k = 3, fill = NA, align = "right"),
            price_ma4 = zoo::rollmean(price, k = 4, fill = NA, align = "right"),
            price_ma6 = zoo::rollmean(price, k = 6, fill = NA, align = "right"),
            price_ma8 = zoo::rollmean(price, k = 8, fill = NA, align = "right"),
            price_ma12 = zoo::rollmean(price, k = 12, fill = NA, align = "right"),
            price_ma26 = zoo::rollmean(price, k = 26, fill = NA, align = "right"),
            price_ma52 = zoo::rollmean(price, k = 52, fill = NA, align = "right"),

            # KAMIS moving averages
            kamis_ma4 = zoo::rollmean(price_kamis, k = 4, fill = NA, align = "right"),
            kamis_ma12 = zoo::rollmean(price_kamis, k = 12, fill = NA, align = "right"),

            # ──────────────────────────────────────────────
            # 3. ROLLING VOLATILITY (std dev)
            # ──────────────────────────────────────────────
            price_vol4 = zoo::rollapply(price,
                width = 4,
                FUN = sd, fill = NA, align = "right"
            ),
            price_vol8 = zoo::rollapply(price,
                width = 8,
                FUN = sd, fill = NA, align = "right"
            ),
            price_vol12 = zoo::rollapply(price,
                width = 12,
                FUN = sd, fill = NA, align = "right"
            ),
            price_vol26 = zoo::rollapply(price,
                width = 26,
                FUN = sd, fill = NA, align = "right"
            ),

            # ──────────────────────────────────────────────
            # 4. ROLLING MIN / MAX
            # ──────────────────────────────────────────────
            price_min4 = zoo::rollapply(price,
                width = 4,
                FUN = min, fill = NA, align = "right"
            ),
            price_max4 = zoo::rollapply(price,
                width = 4,
                FUN = max, fill = NA, align = "right"
            ),
            price_min12 = zoo::rollapply(price,
                width = 12,
                FUN = min, fill = NA, align = "right"
            ),
            price_max12 = zoo::rollapply(price,
                width = 12,
                FUN = max, fill = NA, align = "right"
            ),

            # ──────────────────────────────────────────────
            # 5. PRICE CHANGES & MOMENTUM
            # ──────────────────────────────────────────────
            price_chg1 = price - lag(price, 1),
            price_chg2 = price - lag(price, 2),
            price_chg4 = price - lag(price, 4),
            price_chg8 = price - lag(price, 8),
            price_chg12 = price - lag(price, 12),
            price_chg26 = price - lag(price, 26),
            price_pct1 = (price - lag(price, 1)) / lag(price, 1) * 100,
            price_pct4 = (price - lag(price, 4)) / lag(price, 4) * 100,
            price_pct12 = (price - lag(price, 12)) / lag(price, 12) * 100,

            # Trend: short MA minus long MA (momentum crossover)
            price_momentum4_8 = price_ma4 - price_ma8,
            price_momentum8_26 = price_ma8 - price_ma26,
            price_momentum12_52 = price_ma12 - price_ma52,

            # Acceleration (change in change)
            price_accel = price_chg1 - lag(price_chg1, 1),

            # Short vs long trend direction
            price_trend_short = price_ma4 - lag(price_ma4, 2),
            price_trend_long = price_ma12 - lag(price_ma12, 4),

            # ──────────────────────────────────────────────
            # 6. TRANSFORMED TARGET VERSIONS (for ARIMA / log-diff models)
            # ──────────────────────────────────────────────
            price_log = log(pmax(price, 1e-3)),
            price_log_diff = c(NA, diff(log(pmax(price, 1e-3)))),
            price_diff = c(NA, diff(price)),
            price_boxcox = (pmax(price, 0)^0.782 - 1) / 0.782,

            # ──────────────────────────────────────────────
            # 7. RELATIVE PRICE INDICATORS
            # ──────────────────────────────────────────────
            # Deviation from moving averages
            dev_from_ma4 = (price - price_ma4) / price_ma4 * 100,
            dev_from_ma12 = (price - price_ma12) / price_ma12 * 100,
            dev_from_ma52 = (price - price_ma52) / price_ma52 * 100,

            # Coefficient of variation (rolling)
            price_cv4 = price_vol4 / price_ma4 * 100,
            price_cv12 = price_vol12 / price_ma12 * 100,

            # ──────────────────────────────────────────────
            # 8. KAMIS / agriBORA SPREAD
            # ──────────────────────────────────────────────
            kamis_agri_spread = price_kamis - price,
            kamis_agri_ratio = ifelse(price > 0, price_kamis / price, NA),
            kamis_monthly_dev = price_kamis - price_kamis_monthly
        )

    # ──────────────────────────────────────────────
    # 9. CALENDAR & SEASONAL FEATURES
    # ──────────────────────────────────────────────
    df <- df %>%
        mutate(
            year = year(week),
            week_of_year = isoweek(week),
            day_of_year = yday(week),
            quarter = quarter(week),
            weeks_since_start = as.numeric(
                difftime(week, min(week), units = "weeks")
            ),

            # Cyclical encoding (avoids ordinal discontinuities)
            month_sin = sin(2 * pi * month / 12),
            month_cos = cos(2 * pi * month / 12),
            week_sin = sin(2 * pi * week_of_year / 52),
            week_cos = cos(2 * pi * week_of_year / 52),
            quarter_sin = sin(2 * pi * quarter / 4),
            quarter_cos = cos(2 * pi * quarter / 4),

            # Kenya maize season flags
            # Harvest  : May–June (long rains harvest), Oct (short rains harvest)
            # Lean     : Jan–Feb, Jul–Aug (inter-season peaks)
            is_harvest_season = as.integer(month %in% c(5, 6, 10)),
            is_lean_season = as.integer(month %in% c(1, 2, 7, 8)),
            is_planting_season = as.integer(month %in% c(3, 4, 9)),
            is_long_rains = as.integer(month %in% c(3, 4, 5)),
            is_short_rains = as.integer(month %in% c(10, 11, 12)),
            is_q1 = as.integer(quarter == 1),
            is_q2 = as.integer(quarter == 2),
            is_q3 = as.integer(quarter == 3),
            is_q4 = as.integer(quarter == 4),

            # Price-season cluster (target-encoded later)
            price_season = case_when(
                month %in% c(4, 5, 6) ~ 1, # Harvest / low prices
                month %in% c(7, 8, 9) ~ 2, # Post-harvest
                month %in% c(10, 11, 12) ~ 3, # Lean start
                month %in% c(1, 2, 3) ~ 4 # Lean peak
            ),

            # Month encoding (based on price pattern)
            month_encoded = case_when(
                month %in% c(5, 9) ~ 1L,
                month %in% c(4, 6, 8, 10) ~ 2L,
                month %in% c(11, 12, 2, 3) ~ 3L,
                TRUE ~ 4L
            )
        )

    df
}

# ============================================================
# APPLY TO ALL COUNTIES
# ============================================================
cat("Building feature matrices...\n\n")

county_features <- list()

for (cn in COUNTIES) {
    cat("  ", cn, "... ")
    county_features[[cn]] <- build_features(county_data[[cn]])
    cat(
        nrow(county_features[[cn]]), "weeks ×",
        ncol(county_features[[cn]]) - 3, "features\n"
    )
}

cat("\n✓ 03_feature_engineering.r complete\n")
cat("  Objects: county_features\n\n")
