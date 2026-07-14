# ============================================================
# 01_data_loading.r
# Project : Kenya Maize Price Forecasting (Zindi / agriBORA)
# Purpose : Load KAMIS and agriBORA raw datasets, bind
#           supplementary weeks, and expose clean raw frames.
# Inputs  : kamis_maize_prices.csv, agribora_maize_prices.csv,
#           agriBORA_maize_prices_weeks_46_to_51.csv,
#           agriBORA_Final_Weeks_maize_price.csv
# Outputs : kamis_raw, agri_raw  (global)
# ============================================================

cat("\n========== 01: DATA LOADING ==========\n\n")

# ============================================================
# 1. KAMIS — historical wholesale/retail prices (2021–2025)
#    Columns: County, Date, Market, Classification,
#             Wholesale, Retail, SupplyVolume
# ============================================================
cat("Loading KAMIS data...\n")

kamis_raw <- read_csv(KAMIS_PATH, show_col_types = FALSE)

# Keep only White Maize (target commodity)
kamis_raw <- kamis_raw %>%
    filter(Classification == "White_Maize") %>%
    select(County, Date, Market, Wholesale, Retail, SupplyVolume) %>%
    distinct()

cat("  KAMIS rows (White Maize):", nrow(kamis_raw), "\n")
cat("  KAMIS counties           :", n_distinct(kamis_raw$County), "\n")
cat(
    "  KAMIS date range         :", as.character(min(as.Date(kamis_raw$Date))),
    "→", as.character(max(as.Date(kamis_raw$Date))), "\n\n"
)

# ============================================================
# 2. agriBORA — weekly wholesale transaction prices (2023–2025)
#    Columns: County, Date, WholeSale
# ============================================================
cat("Loading agriBORA data...\n")

agri_base <- read_csv(AGRI_PATH, show_col_types = FALSE)
agri_sup <- read_csv(AGRI_SUP_PATH, show_col_types = FALSE)
agri_raw <- bind_rows(agri_base, agri_sup) %>% distinct()

cat("  agriBORA rows (base + weeks 46-51):", nrow(agri_raw), "\n")
cat("  agriBORA counties                  :", n_distinct(agri_raw$County), "\n")
cat(
    "  agriBORA date range                :",
    as.character(min(as.Date(agri_raw$Date))), "→",
    as.character(max(as.Date(agri_raw$Date))), "\n\n"
)

# ============================================================
# 3. Final scoring weeks (truth data released post-competition)
# ============================================================
cat("Loading final scoring weeks...\n")

agri_final <- tryCatch(
    read_csv(AGRI_FINAL_PATH, show_col_types = FALSE),
    error = function(e) {
        cat("  ⚠ Final weeks file not found — skipping\n")
        NULL
    }
)

if (!is.null(agri_final)) {
    cat("  Final weeks rows:", nrow(agri_final), "\n\n")
    # Bind into agri_raw for evaluation if available
    agri_raw <- bind_rows(agri_raw, agri_final) %>% distinct()
}

# ============================================================
# 4. Sample submission — load to confirm ID format
# ============================================================
sample_sub <- tryCatch(
    read_csv(SAMPLE_SUB_PATH, show_col_types = FALSE),
    error = function(e) NULL
)

if (!is.null(sample_sub)) {
    cat("Sample submission format:\n")
    print(head(sample_sub, 3))
    cat("\n")
}

cat("✓ 01_data_loading.r complete\n")
cat("  Objects created: kamis_raw, agri_raw\n\n")
