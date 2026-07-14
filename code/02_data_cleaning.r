# ============================================================
# 02_data_cleaning.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Snap dates to Monday-week boundaries, impute
#           missing KAMIS Wholesale/SupplyVolume via regression,
#           compute volume-weighted weekly prices, calibrate
#           KAMIS to agriBORA scale, LOCF-fill gaps, merge
#           all counties into one master panel.
# Requires: kamis_raw, agri_raw  (01_data_loading.r)
# Outputs : panel  — long-format weekly price panel
#           county_data — named list, one tibble per county
# ============================================================

cat("\n========== 02: DATA CLEANING ==========\n\n")

# ============================================================
# HELPER — build a clean weekly series for ONE county
#   Combines KAMIS (volume-weighted wholesale) + agriBORA
#   (target price).  agriBORA is the truth; KAMIS is used
#   as an auxiliary feature and to back-fill early periods.
# ============================================================
build_county_series <- function(county_name, kamis_raw, agri_raw) {
    # --------------------------------------------------------
    # A. KAMIS: snap, impute, volume-weight aggregate
    # --------------------------------------------------------
    kf <- kamis_raw %>%
        filter(County == county_name) %>%
        mutate(
            Date         = snap_to_monday(as.Date(Date)),
            week         = floor_date(Date, unit = "week", week_start = 1),
            Wholesale    = as.numeric(Wholesale),
            Retail       = as.numeric(Retail),
            SupplyVolume = as.numeric(SupplyVolume)
        ) %>%
        select(week, Market, Wholesale, Retail, SupplyVolume)

    if (nrow(kf) == 0) {
        cat("  ⚠", county_name, ": no KAMIS rows found\n")
        kamis_weekly <- tibble(week = as.Date(character()), price_kamis = numeric())
    } else {
        # Impute missing Wholesale via Market + Retail regression
        if (any(is.na(kf$Wholesale))) {
            imp_lm <- tryCatch(
                lm(Wholesale ~ Market + Retail, data = kf, na.action = na.omit),
                error = function(e) NULL
            )
            if (!is.null(imp_lm)) {
                miss_idx <- which(is.na(kf$Wholesale))
                kf$Wholesale[miss_idx] <- predict(
                    imp_lm,
                    newdata = kf[miss_idx, ]
                )
            } else {
                kf$Wholesale[is.na(kf$Wholesale)] <- median(
                    kf$Wholesale,
                    na.rm = TRUE
                )
            }
        }

        # Impute missing SupplyVolume via Market + Wholesale regression
        if (any(is.na(kf$SupplyVolume) | is.infinite(kf$SupplyVolume))) {
            kf$SupplyVolume[is.infinite(kf$SupplyVolume)] <- NA
            imp_sv <- tryCatch(
                lm(SupplyVolume ~ Market + Wholesale, data = kf, na.action = na.omit),
                error = function(e) NULL
            )
            if (!is.null(imp_sv)) {
                miss_idx <- which(is.na(kf$SupplyVolume))
                kf$SupplyVolume[miss_idx] <- pmax(
                    predict(imp_sv, newdata = kf[miss_idx, ]), 1
                )
            } else {
                kf$SupplyVolume[is.na(kf$SupplyVolume)] <- 1
            }
        }

        # Volume-weighted price per market-week, then average across markets
        kamis_market <- kf %>%
            filter(!is.na(SupplyVolume) & SupplyVolume > 0) %>%
            group_by(week, Market) %>%
            summarise(
                price     = mean(Wholesale, na.rm = TRUE),
                vol       = mean(SupplyVolume, na.rm = TRUE),
                .groups   = "drop"
            ) %>%
            mutate(val = price * vol)

        kamis_weekly <- kamis_market %>%
            group_by(week) %>%
            summarise(
                price_kamis = sum(val, na.rm = TRUE) /
                    sum(vol, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            arrange(week)
    }

    # --------------------------------------------------------
    # B. agriBORA: clean and aggregate to weekly
    # --------------------------------------------------------
    af <- agri_raw %>%
        filter(County == county_name) %>%
        mutate(
            Date = as.Date(Date),
            week = floor_date(snap_to_monday(Date),
                unit = "week", week_start = 1
            ),
            price = as.numeric(WholeSale)
        ) %>%
        filter(!is.na(price)) %>%
        group_by(week) %>%
        summarise(price = mean(price, na.rm = TRUE), .groups = "drop") %>%
        arrange(week)

    # --------------------------------------------------------
    # C. Calibrate KAMIS to agriBORA scale
    #    ratio = median(agri / kamis) on overlapping weeks
    # --------------------------------------------------------
    overlap <- inner_join(kamis_weekly, af,
        by = "week",
        suffix = c("_kamis", "_agri")
    ) %>%
        filter(!is.na(price_kamis) & price_kamis > 0)

    if (nrow(overlap) >= 5) {
        cal_model <- lm(price ~ price_kamis, data = overlap)
        kamis_weekly <- kamis_weekly %>%
            mutate(price_kamis_cal = pmax(
                predict(cal_model, newdata = kamis_weekly), 0
            ))
    } else {
        ratio <- if (nrow(overlap) > 0) {
            median(overlap$price / overlap$price_kamis, na.rm = TRUE)
        } else {
            1.0
        }
        kamis_weekly <- kamis_weekly %>%
            mutate(price_kamis_cal = price_kamis * ratio)
    }

    # --------------------------------------------------------
    # D. Build full weekly grid, merge, LOCF fill
    # --------------------------------------------------------
    all_weeks <- seq(
        from = min(c(kamis_weekly$week, af$week)),
        to   = max(c(kamis_weekly$week, af$week)),
        by   = "week"
    )

    grid <- tibble(week = all_weeks) %>%
        left_join(af, by = "week") %>%
        left_join(kamis_weekly, by = "week")

    # LOCF fill agriBORA gaps using calibrated KAMIS where possible
    grid <- grid %>%
        mutate(
            price_filled = ifelse(!is.na(price), price,
                ifelse(!is.na(price_kamis_cal), price_kamis_cal, NA)
            ),
            price_filled = locf_fill(price_filled),
            data_source = case_when(
                !is.na(price) ~ "agriBORA",
                !is.na(price_kamis_cal) ~ "KAMIS_calibrated",
                TRUE ~ "LOCF"
            )
        )

    # Monthly average KAMIS (auxiliary)
    grid <- grid %>%
        mutate(month = month(week)) %>%
        group_by(month) %>%
        mutate(price_kamis_monthly = mean(price_kamis, na.rm = TRUE)) %>%
        ungroup()

    grid %>%
        mutate(county = county_name) %>%
        select(county, week, month,
            price        = price_filled,
            price_raw    = price,
            price_kamis,
            price_kamis_cal,
            price_kamis_monthly,
            data_source
        )
}

# ============================================================
# PROCESS ALL 5 COUNTIES
# ============================================================
cat("Building per-county weekly series...\n\n")

county_data <- list()

for (cn in COUNTIES) {
    cat("  Processing:", cn, "... ")
    county_data[[cn]] <- build_county_series(cn, kamis_raw, agri_raw)
    n <- nrow(county_data[[cn]])
    r <- sum(county_data[[cn]]$data_source == "agriBORA")
    cat(
        n, "weeks |", r, "agriBORA rows |",
        n - r, "filled\n"
    )
}

# ============================================================
# COMBINED PANEL (long format)
# ============================================================
panel <- bind_rows(county_data) %>%
    arrange(county, week)

cat("\nPanel shape:", nrow(panel), "rows ×", ncol(panel), "cols\n")
cat("Counties   :", paste(unique(panel$county), collapse = ", "), "\n")
cat(
    "Date range :", as.character(min(panel$week)), "→",
    as.character(max(panel$week)), "\n\n"
)

cat("✓ 02_data_cleaning.r complete\n")
cat("  Objects: panel, county_data\n\n")
