# ============================================================
# 06_evaluation.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Walk-forward cross-validation, per-county
#           leaderboard, feature importance ranking.
# Requires: county_features, county_train, county_predictions
# Outputs : leaderboard_df, cv_results_df (printed & stored)
# ============================================================

cat("\n========== 06: EVALUATION ==========\n\n")

# ============================================================
# 1. HOLDOUT LEADERBOARD  (last 8 weeks as test)
# ============================================================
cat("1. Holdout leaderboard (last", HOLD_OUT_WEEKS, "weeks)...\n\n")

leaderboard_rows <- list()

for (cn in COUNTIES) {
    preds <- county_predictions[[cn]]
    if (is.null(preds)) next

    pred_cols <- setdiff(names(preds), c("week", "price_actual"))

    for (mdl in pred_cols) {
        p <- preds[[mdl]]
        a <- preds$price_actual
        if (all(is.na(p))) next

        s <- competition_score(a, p)
        leaderboard_rows[[paste(cn, mdl, sep = ":")]] <- data.frame(
            county = cn,
            model = mdl,
            mae = round(s$mae, 2),
            rmse = round(s$rmse, 2),
            score = round(s$score, 2),
            stringsAsFactors = FALSE
        )
    }
}

leaderboard_df <- do.call(rbind, leaderboard_rows)
rownames(leaderboard_df) <- NULL

if (!is.null(leaderboard_df) && nrow(leaderboard_df) > 0) {
    leaderboard_df <- leaderboard_df[order(leaderboard_df$score), ]

    cat("══════════════════════════════════════════════════════\n")
    cat("  HOLDOUT LEADERBOARD (lower score = better)\n")
    cat("══════════════════════════════════════════════════════\n")
    print(leaderboard_df, row.names = FALSE)
    cat("\n")

    # Best model per county
    cat("Best model per county:\n")
    best_per_county <- leaderboard_df %>%
        group_by(county) %>%
        slice_min(order_by = score, n = 1) %>%
        ungroup()
    print(best_per_county, row.names = FALSE)
    cat("\n")
}

# ============================================================
# 2. WALK-FORWARD CROSS-VALIDATION  (LightGBM, core features)
# ============================================================
cat("2. Walk-forward cross-validation (LightGBM, core features)...\n\n")

cv_results <- list()

for (cn in COUNTIES) {
    df <- county_features[[cn]] %>% arrange(week)
    feats <- county_features_clean[[cn]]$core
    windows <- make_cv_windows(df, n_windows = 5, horizon = 2)

    fold_scores <- numeric(length(windows))

    for (w_idx in seq_along(windows)) {
        win <- windows[[w_idx]]
        tr_win <- df[win$train, ]
        te_win <- df[win$test, ]

        tryCatch(
            {
                m <- train_lgbm(tr_win, feats, nrounds = 300)
                p <- predict_lgbm(m, te_win)
                fold_scores[w_idx] <- score_metric(te_win$price, p)
            },
            error = function(e) {
                fold_scores[w_idx] <<- NA
            }
        )
    }

    cv_results[[cn]] <- fold_scores
    cat(
        "  ", cn, "| CV scores:",
        paste(round(fold_scores, 2), collapse = ", "),
        "| Mean:", round(mean(fold_scores, na.rm = TRUE), 2), "\n"
    )
}

cat("\n")

cv_results_df <- data.frame(
    county     = COUNTIES,
    cv_mean    = sapply(COUNTIES, function(cn) mean(cv_results[[cn]], na.rm = TRUE)),
    cv_sd      = sapply(COUNTIES, function(cn) sd(cv_results[[cn]], na.rm = TRUE))
) %>% arrange(cv_mean)

cat("Cross-validation summary:\n")
print(cv_results_df, row.names = FALSE)
cat("\n")

# ============================================================
# 3. FEATURE IMPORTANCE  (LightGBM, all features, per county)
# ============================================================
cat("3. Feature importance (LightGBM, all features)...\n\n")

fi_list <- list()

for (cn in COUNTIES) {
    mdl_obj <- county_models[[cn]]$lgbm_all
    if (is.null(mdl_obj)) next

    fi <- lgb.importance(mdl_obj$model, percentage = TRUE)
    fi$county <- cn
    fi_list[[cn]] <- fi
}

if (length(fi_list) > 0) {
    fi_all <- bind_rows(fi_list)

    # Average gain across counties
    fi_avg <- fi_all %>%
        group_by(Feature) %>%
        summarise(avg_gain = mean(Gain, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(avg_gain))

    cat("Top 20 features (average importance across counties):\n")
    print(head(fi_avg, 20), row.names = FALSE)
    cat("\n")
}

cat("✓ 06_evaluation.r complete\n")
cat("  Objects: leaderboard_df, cv_results_df, fi_avg\n\n")
