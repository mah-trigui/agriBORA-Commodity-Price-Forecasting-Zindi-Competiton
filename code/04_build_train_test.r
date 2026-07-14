# ============================================================
# 04_build_train_test.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Drop multicollinear / low-variance features,
#           define canonical feature lists, apply chronological
#           train/test split, and build modelling-ready
#           feature matrices for each county.
# Requires: county_features  (03_feature_engineering.r)
# Outputs : county_train, county_test, FEAT_SETS (feature lists)
# ============================================================

cat("\n========== 04: BUILD TRAIN/TEST SPLIT ==========\n\n")

# ============================================================
# 1. CANONICAL FEATURE LISTS
#    Ordered by importance (from Feat Select.r analysis)
# ============================================================

# All lag + rolling + seasonal features
FEAT_ALL <- c(
    # Lag prices
    "price_lag1", "price_lag2", "price_lag3", "price_lag4",
    "price_lag5", "price_lag6", "price_lag8", "price_lag12",
    "price_lag16", "price_lag26", "price_lag52",
    # KAMIS auxiliary lags
    "kamis_lag1", "kamis_lag2", "kamis_lag4",
    "kamis_ma4", "kamis_ma12",
    "price_kamis_monthly",
    # Moving averages
    "price_ma2", "price_ma3", "price_ma4", "price_ma6",
    "price_ma8", "price_ma12", "price_ma26", "price_ma52",
    # Volatility
    "price_vol4", "price_vol8", "price_vol12", "price_vol26",
    # Min/Max
    "price_min4", "price_max4", "price_min12", "price_max12",
    # Momentum & changes
    "price_chg1", "price_chg2", "price_chg4", "price_chg8",
    "price_pct1", "price_pct4", "price_pct12",
    "price_momentum4_8", "price_momentum8_26", "price_momentum12_52",
    "price_accel", "price_trend_short", "price_trend_long",
    # Relative indicators
    "dev_from_ma4", "dev_from_ma12", "dev_from_ma52",
    "price_cv4", "price_cv12",
    "kamis_agri_ratio", "kamis_monthly_dev",
    # Calendar
    "week_of_year", "month", "quarter",
    "month_sin", "month_cos", "week_sin", "week_cos",
    "quarter_sin", "quarter_cos",
    "is_harvest_season", "is_lean_season", "is_planting_season",
    "is_long_rains", "is_short_rains",
    "price_season", "month_encoded",
    "weeks_since_start"
)

# Parsimonious set — top predictors only
FEAT_CORE <- c(
    "price_lag1", "price_lag2", "price_lag4", "price_lag8",
    "price_lag12", "price_lag52",
    "price_ma4", "price_ma8", "price_ma12", "price_ma26",
    "price_vol4", "price_vol12",
    "price_chg1", "price_chg4", "price_pct4",
    "price_momentum4_8", "price_momentum12_52",
    "kamis_lag1", "price_kamis_monthly",
    "month_sin", "month_cos", "week_sin", "week_cos",
    "is_harvest_season", "is_lean_season", "price_season",
    "weeks_since_start"
)

# Minimal set (ARIMA / ETS supplement)
FEAT_MINIMAL <- c(
    "price_lag1", "price_lag2", "price_lag4",
    "price_ma4", "price_ma12",
    "is_harvest_season", "is_lean_season",
    "month_sin", "month_cos"
)

FEAT_SETS <- list(all = FEAT_ALL, core = FEAT_CORE, minimal = FEAT_MINIMAL)

# ============================================================
# 2. MULTICOLLINEARITY REMOVAL PER COUNTY
#    Drop features with Pearson |r| ≥ 0.90 with another
#    feature, keeping the one most correlated with price.
# ============================================================
remove_multicollinear <- function(df, feat_list, target = "price",
                                  threshold = 0.90) {
    feats <- feat_list[feat_list %in% names(df)]
    data_mc <- df[, feats] %>% na.omit()

    if (ncol(data_mc) < 2) {
        return(feats)
    }

    cor_mat <- cor(data_mc, use = "complete.obs")

    # Target correlations
    tgt_cor <- sapply(feats, function(f) {
        valid <- !is.na(df[[f]]) & !is.na(df[[target]])
        if (sum(valid) < 5) {
            return(0)
        }
        abs(cor(df[[f]][valid], df[[target]][valid]))
    })

    pairs <- which(abs(cor_mat) >= threshold & abs(cor_mat) < 1, arr.ind = TRUE)
    pairs <- pairs[pairs[, 1] < pairs[, 2], , drop = FALSE]
    to_drop <- character(0)

    for (i in seq_len(nrow(pairs))) {
        v1 <- rownames(cor_mat)[pairs[i, 1]]
        v2 <- colnames(cor_mat)[pairs[i, 2]]
        if (v1 %in% to_drop || v2 %in% to_drop) next
        drop_v <- if (tgt_cor[v1] < tgt_cor[v2]) v1 else v2
        to_drop <- c(to_drop, drop_v)
    }

    setdiff(feats, to_drop)
}

# ============================================================
# 3. CHRONOLOGICAL SPLIT
#    Use last 8 weeks as local test (validation); rest = train
# ============================================================
HOLD_OUT_WEEKS <- 8

county_train <- list()
county_test <- list()
county_features_clean <- list()

cat("Splitting data chronologically (holdout =", HOLD_OUT_WEEKS, "weeks)...\n\n")

for (cn in COUNTIES) {
    df <- county_features[[cn]] %>% arrange(week)

    # Remove features with >80% missing
    valid_feats <- FEAT_ALL[FEAT_ALL %in% names(df)]
    miss_rate <- sapply(df[, valid_feats], function(x) mean(is.na(x)))
    valid_feats <- valid_feats[miss_rate < 0.80]

    # Rebuild feature sets with valid columns only
    feat_all_cn <- remove_multicollinear(df, valid_feats)
    feat_core_cn <- remove_multicollinear(df, FEAT_CORE[FEAT_CORE %in% valid_feats])
    feat_min_cn <- FEAT_MINIMAL[FEAT_MINIMAL %in% valid_feats]

    county_features_clean[[cn]] <- list(
        all     = feat_all_cn,
        core    = feat_core_cn,
        minimal = feat_min_cn
    )

    n_total <- nrow(df)
    train_rows <- 1:(n_total - HOLD_OUT_WEEKS)
    test_rows <- (n_total - HOLD_OUT_WEEKS + 1):n_total

    county_train[[cn]] <- df[train_rows, ]
    county_test[[cn]] <- df[test_rows, ]

    cat(
        "  ", cn, ":", length(train_rows), "train /",
        length(test_rows), "test |",
        length(feat_all_cn), "features (all) |",
        length(feat_core_cn), "features (core)\n"
    )
}

cat("\n✓ 04_build_train_test.r complete\n")
cat("  Objects: county_train, county_test, county_features_clean, FEAT_SETS\n\n")
