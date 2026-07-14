# ============================================================
# 05_models.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Train per-county forecasting models across multiple
#           target transformations and feature sets.
#           Models: LightGBM, XGBoost, ARIMA, Prophet, ETS,
#           Naive (seasonal), and a stacked ensemble.
# Requires: county_train, county_test, county_features_clean
# Outputs : county_predictions (named list of prediction dfs)
#           county_models      (named list of trained models)
# ============================================================

cat("\n========== 05: MODELS ==========\n\n")

# ============================================================
# COMPETITION METRIC SHORTHAND
# ============================================================
score_metric <- function(actual, predicted) {
    mae <- mean(abs(actual - predicted), na.rm = TRUE)
    rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    0.5 * mae + 0.5 * rmse
}

# ============================================================
# TARGET TRANSFORMATIONS
# ============================================================
apply_transform <- function(y, type) {
    switch(type,
        original  = y,
        log       = log(pmax(y, 1e-3)),
        log_diff  = c(NA, diff(log(pmax(y, 1e-3)))),
        diff      = c(NA, diff(y)),
        boxcox    = (pmax(y, 0)^0.782 - 1) / 0.782
    )
}

invert_transform <- function(pred, type, last_train_price) {
    switch(type,
        original = pred,
        log = exp(pred),
        log_diff = {
            # Cumulative sum to recover log prices, then exp
            last_log <- log(pmax(last_train_price, 1e-3))
            exp(cumsum(c(last_log, pred))[-1])
        },
        diff = last_train_price + cumsum(pred),
        boxcox = pmax((pred * 0.782 + 1)^(1 / 0.782), 0)
    )
}

# ============================================================
# HELPER — build ML feature matrix for a period
# ============================================================
get_xy <- function(data, feat_list, target_col = "price") {
    feats <- feat_list[feat_list %in% names(data)]
    X <- as.matrix(data[, feats])
    y <- data[[target_col]]
    X[is.na(X)] <- 0
    list(X = X, y = y, feats = feats)
}

# ============================================================
# MODEL 1: LightGBM
# ============================================================
train_lgbm <- function(train, feat_list, target_col = "price",
                       nrounds = 500, params = NULL) {
    xy <- get_xy(train, feat_list, target_col)
    idx <- !is.na(xy$y)
    X <- xy$X[idx, ]
    y <- xy$y[idx]

    default_p <- list(
        objective        = "regression",
        metric           = "mae",
        num_leaves       = 31,
        learning_rate    = 0.05,
        feature_fraction = 0.8,
        bagging_fraction = 0.8,
        bagging_freq     = 5,
        min_data_in_leaf = 5,
        verbose          = -1
    )
    if (!is.null(params)) default_p <- modifyList(default_p, params)

    dtrain <- lgb.Dataset(X, label = y)
    model <- lgb.train(
        params = default_p, data = dtrain,
        nrounds = nrounds, verbose = -1
    )
    list(model = model, feats = xy$feats, type = "lgbm")
}

predict_lgbm <- function(model_obj, test) {
    X <- as.matrix(test[, model_obj$feats])
    X[is.na(X)] <- 0
    predict(model_obj$model, X)
}

# ============================================================
# MODEL 2: XGBoost
# ============================================================
train_xgb <- function(train, feat_list, target_col = "price",
                      nrounds = 500, params = NULL) {
    xy <- get_xy(train, feat_list, target_col)
    idx <- !is.na(xy$y)
    X <- xy$X[idx, ]
    y <- xy$y[idx]

    default_p <- list(
        objective = "reg:squarederror",
        max_depth = 6, eta = 0.05,
        subsample = 0.8, colsample_bytree = 0.8,
        min_child_weight = 3
    )
    if (!is.null(params)) default_p <- modifyList(default_p, params)

    dtrain <- xgb.DMatrix(data = X, label = y)
    model <- xgb.train(
        params = default_p, data = dtrain,
        nrounds = nrounds, verbose = 0
    )
    list(model = model, feats = xy$feats, type = "xgb")
}

predict_xgb <- function(model_obj, test) {
    X <- as.matrix(test[, model_obj$feats])
    X[is.na(X)] <- 0
    predict(model_obj$model, xgb.DMatrix(data = X))
}

# ============================================================
# MODEL 3: Auto-ARIMA
# ============================================================
train_arima <- function(train, target_col = "price") {
    y <- train[[target_col]]
    idx <- !is.na(y)
    y <- y[idx]
    ts_obj <- ts(y, frequency = 52)
    model <- tryCatch(
        forecast::auto.arima(ts_obj,
            seasonal = TRUE,
            stepwise = TRUE, approximation = TRUE
        ),
        error = function(e) forecast::auto.arima(ts(y, frequency = 1))
    )
    list(
        model = model, last_price = tail(train$price, 1),
        n_train = sum(idx), type = "arima"
    )
}

predict_arima <- function(model_obj, h = 2) {
    fc <- forecast::forecast(model_obj$model, h = h)
    as.numeric(fc$mean)
}

# ============================================================
# MODEL 4: ETS (Exponential Smoothing)
# ============================================================
train_ets <- function(train, target_col = "price") {
    y <- train[[target_col]]
    y <- y[!is.na(y)]
    ts_obj <- ts(y, frequency = 52)
    model <- tryCatch(
        forecast::ets(ts_obj),
        error = function(e) forecast::ets(ts(y, frequency = 1))
    )
    list(model = model, last_price = tail(train$price, 1), type = "ets")
}

predict_ets <- function(model_obj, h = 2) {
    fc <- forecast::forecast(model_obj$model, h = h)
    as.numeric(fc$mean)
}

# ============================================================
# MODEL 5: Prophet
# ============================================================
train_prophet <- function(train, target_col = "price") {
    pd <- train %>%
        select(ds = week, y = all_of(target_col)) %>%
        filter(!is.na(y))
    model <- tryCatch(
        prophet::prophet(pd,
            yearly.seasonality = TRUE,
            weekly.seasonality = FALSE,
            daily.seasonality = FALSE,
            seasonality.mode = "multiplicative",
            verbose = FALSE
        ),
        error = function(e) {
            prophet::prophet(pd, daily.seasonality = FALSE, verbose = FALSE)
        }
    )
    list(model = model, type = "prophet")
}

predict_prophet <- function(model_obj, future_weeks) {
    future_df <- data.frame(ds = as.Date(future_weeks))
    fc <- predict(model_obj$model, future_df)
    pmax(fc$yhat, 0)
}

# ============================================================
# MODEL 6: Seasonal Naïve (baseline)
# ============================================================
predict_snaive <- function(train, h = 2, lag = 52) {
    y <- train$price
    n <- length(y)
    sapply(seq_len(h), function(i) {
        look_back <- n - lag + i
        if (look_back > 0 && look_back <= n) y[look_back] else tail(y, 1)
    })
}

# ============================================================
# TRAIN ALL MODELS FOR ALL COUNTIES
# ============================================================
county_models <- list()
county_predictions <- list()

for (cn in COUNTIES) {
    cat(strrep("─", 40), "\n")
    cat(" COUNTY:", cn, "\n")
    cat(strrep("─", 40), "\n\n")

    train <- county_train[[cn]]
    test <- county_test[[cn]]
    feats <- county_features_clean[[cn]]
    models <- list()
    preds <- data.frame(week = test$week, price_actual = test$price)

    # ── LightGBM (all features) ──────────────────────────
    tryCatch(
        {
            m <- train_lgbm(train, feats$all, nrounds = 500)
            p <- predict_lgbm(m, test)
            preds$lgbm_all <- p
            models$lgbm_all <- m
            cat(
                "  ✓ LightGBM (all)  score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ LightGBM (all):", e$message, "\n")
    )

    # ── LightGBM (core features) ─────────────────────────
    tryCatch(
        {
            m <- train_lgbm(train, feats$core, nrounds = 500)
            p <- predict_lgbm(m, test)
            preds$lgbm_core <- p
            models$lgbm_core <- m
            cat(
                "  ✓ LightGBM (core) score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ LightGBM (core):", e$message, "\n")
    )

    # ── XGBoost (core features) ──────────────────────────
    tryCatch(
        {
            m <- train_xgb(train, feats$core, nrounds = 500)
            p <- predict_xgb(m, test)
            preds$xgb_core <- p
            models$xgb_core <- m
            cat(
                "  ✓ XGBoost (core)  score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ XGBoost (core):", e$message, "\n")
    )

    # ── LightGBM on log-diff target ──────────────────────
    tryCatch(
        {
            ld_train <- train %>%
                mutate(price_ld = apply_transform(price, "log_diff")) %>%
                filter(!is.na(price_ld))
            m <- train_lgbm(ld_train, feats$core,
                target_col = "price_ld",
                nrounds = 300
            )
            p_ld <- predict_lgbm(m, test %>%
                mutate(price_ld = apply_transform(price, "log_diff")) %>%
                {
                    replace(., is.na(.[["price_ld"]]), 0)
                })
            p <- invert_transform(p_ld, "log_diff",
                last_train_price = tail(train$price, 1)
            )
            preds$lgbm_logdiff <- p
            models$lgbm_logdiff <- list(model = m, type = "lgbm_logdiff")
            cat(
                "  ✓ LightGBM (log-diff) score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ LightGBM (log-diff):", e$message, "\n")
    )

    # ── ARIMA ────────────────────────────────────────────
    tryCatch(
        {
            m <- train_arima(train)
            p <- predict_arima(m, h = HOLD_OUT_WEEKS)
            preds$arima <- p
            models$arima <- m
            cat(
                "  ✓ ARIMA            score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ ARIMA:", e$message, "\n")
    )

    # ── ETS ──────────────────────────────────────────────
    tryCatch(
        {
            m <- train_ets(train)
            p <- predict_ets(m, h = HOLD_OUT_WEEKS)
            preds$ets <- p
            models$ets <- m
            cat(
                "  ✓ ETS              score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ ETS:", e$message, "\n")
    )

    # ── Prophet ──────────────────────────────────────────
    tryCatch(
        {
            m <- train_prophet(train)
            p <- predict_prophet(m, test$week)
            preds$prophet <- p
            models$prophet <- m
            cat(
                "  ✓ Prophet          score:",
                round(score_metric(test$price, p), 2), "\n"
            )
        },
        error = function(e) cat("  ✗ Prophet:", e$message, "\n")
    )

    # ── Seasonal Naïve ───────────────────────────────────
    p_naive <- predict_snaive(train, h = HOLD_OUT_WEEKS)
    preds$snaive <- p_naive
    cat(
        "  ✓ Seasonal Naïve   score:",
        round(score_metric(test$price, p_naive), 2), "\n"
    )

    # ── Ensemble (mean of ML + TS models) ────────────────
    pred_cols <- setdiff(names(preds), c("week", "price_actual"))
    if (length(pred_cols) >= 2) {
        pred_mat <- as.matrix(preds[, pred_cols])
        p_ens <- rowMeans(pred_mat, na.rm = TRUE)
        preds$ensemble <- p_ens
        cat(
            "  ✓ Ensemble         score:",
            round(score_metric(test$price, p_ens), 2), "\n"
        )
    }

    county_models[[cn]] <- models
    county_predictions[[cn]] <- preds
    cat("\n")
}

cat("✓ 05_models.r complete\n")
cat("  Objects: county_models, county_predictions\n\n")
