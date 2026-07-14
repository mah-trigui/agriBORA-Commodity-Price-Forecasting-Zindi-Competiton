# Kenya Maize Price Forecasting — Pipeline

This competition is hosted on Zindi, a machine learning platform for data science challenges.  
Here is the link to the competition: [agriBORA Commodity Price Forecasting Challenge 🌾 - Win €8,250 EUR](https://zindi.africa/competitions/agriBORA-Commodity-Price-Forecasting-Challenge)

I was ranked in the TOP 24% (only 354 out of 1248 participants managed to submit)!

---

Zindi competition · agriBORA × KAMIS · Weekly price prediction for 5 Kenya counties.

---

## Competition Overview

| Item | Details |
|---|---|
| Task | Predict the weekly average wholesale maize price (KES) |
| Target counties | Kiambu, Kirinyaga, Mombasa, Nairobi, Uasin-Gishu |
| Horizon | 2 weeks ahead, rolling |
| Metric | `score = 0.5 × MAE + 0.5 × RMSE` (lower is better) |
| Submission format | One row per `{County}_Week_{iso_week}` |
| Both target columns | `Target_RMSE` and `Target_MAE` hold the same predicted price |
| Seed | `1618` |

---

## Data Sources

| File | Description |
|---|---|
| `kamis_maize_prices.csv` | KAMIS daily market prices — filtered to `Classification == "White_Maize"` |
| `agribora_maize_prices.csv` | agriBORA weekly transaction prices (main series) |
| `agriBORA_maize_prices_weeks_46_to_51.csv` | Extended agriBORA weeks 46–51 |
| `agriBORA_Final_Weeks_maize_price.csv` | Final scoring truth (optional) |
| `SampleSubmission.csv` | Submission format reference |

All data files must be placed in `DATA_DIR` (default: `C:/Users/mtrigui2/Desktop/Z1`).

---

## Pipeline Structure

```
pipeline_maize/
├── 00_config.r                 Libraries, paths, constants, helper functions
├── 01_data_loading.r           Load KAMIS + agriBORA raw data
├── 02_data_cleaning.r          Snap to Monday, impute, calibrate, LOCF fill
├── 03_feature_engineering.r    Lags, rolling stats, seasonality, momentum
├── 04_build_train_test.r       Multicollinearity removal, chronological split
├── 05_models.r                 LightGBM, XGBoost, ARIMA, ETS, Prophet, Ensemble
├── 06_evaluation.r             Walk-forward CV, holdout leaderboard, feature importance
├── 07_submission.r             Rolling 2-week forecast → submission CSV
├── MAIN.r                      Orchestration — source this to run everything
└── README.md                   This file
```

---

## Quick Start

```r
# Run the full pipeline from R console
source("pipeline_maize/MAIN.r")
```

The submission CSV is written to `SUBMIT_DIR` (defaults to `pipeline_maize/submissions/`).

---

## Key Design Decisions

### Data Processing
- **KAMIS dates** are snapped to the nearest Monday (ISO-week alignment)
- **Wholesale NA imputation**: `lm(Wholesale ~ Market + Retail)` per county
- **SupplyVolume NA imputation**: `lm(SupplyVolume ~ Market + Wholesale)`
- **Volume-weighted aggregation**: Weekly KAMIS price = `sum(Wholesale × SupplyVolume) / sum(SupplyVolume)`
- **KAMIS → agriBORA calibration**: Linear regression on overlapping weeks; KAMIS is then used only as an external feature
- **agriBORA** is the **target** series; gaps are filled via LOCF

### Feature Engineering
- **Lags**: 1, 2, 3, 4, 5, 6, 8, 12, 16, 26, 52 weeks
- **Rolling MA**: 2, 3, 4, 6, 8, 12, 26, 52 weeks
- **Rolling Volatility**: 4, 8, 12, 26 weeks
- **Momentum**: MA4−MA8, MA8−MA26, MA12−MA52
- **Seasonal flags**: `is_harvest_season` (May/Jun/Oct), `is_lean_season` (Jan/Feb/Jul/Aug)
- **Cyclical encoding**: `sin/cos` of month, week-of-year, quarter

### Validation
- Walk-forward CV with 5 windows, 2-week horizon, minimum 52 weeks training
- Holdout: last 8 weeks held out for final model selection

### Modelling
| Model | Key settings |
|---|---|
| LightGBM | `num_leaves=31`, `lr=0.05`, 500 rounds |
| XGBoost | `max_depth=6`, `eta=0.05`, 500 rounds |
| Auto-ARIMA | `seasonal=TRUE`, frequency=52 |
| ETS | Automatic damped exponential smoothing |
| Prophet | `yearly.seasonality=TRUE`, multiplicative mode |
| Ensemble | Equal-weight mean of all available forecasts |

---

## Requirements

```r
install.packages(c(
    "tidyverse", "lubridate", "zoo", "imputeTS", "forecast",
    "prophet", "xgboost", "lightgbm", "glmnet", "caret"
))
```
