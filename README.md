# NVDA Time Series Forecasting (ARMA) & Fixed Effects Panel Data Analysis

This repository contains econometrics projects in **R**, focusing on both time series forecasting and panel data analysis.  
The goal is to identify patterns, select optimal models, and evaluate forecasting and regression accuracy.

---

## 1. NVDA Time Series Forecasting (ARMA)

### Methods
- Data exploration and visualization across multiple years
- Stationarity check (ADF test)
- Autocorrelation and Partial Autocorrelation analysis (ACF/PACF)
- ARMA model selection (based on AIC)
- Forecast evaluation with RMSE and MAE

### Files
- `NVDA_ARMA_Forecast.R` – R script containing the full workflow  
- `data_nvda.csv` – dataset with NVDA stock daily prices (source: Yahoo Finance)  
- `plots/` – saved figures of diagnostics and forecasts  

### Key Outcomes
- Identified changes and trends in NVDA stock across years  
- Selected the optimal ARMA model for forecasting  
- Generated forecasts and compared them to actual values with visual plots  

---

## 2. Fixed Effects Model (Panel Data)

### Methods
- Panel data setup with multiple entities observed across time  
- Fixed Effects regression model to control for individual heterogeneity  
- Comparison of model results with alternative specifications  

### File
- `Fixed_Effects_Model.R` – R script implementing the Fixed Effects regression  

### Key Outcomes
- Demonstrated how Fixed Effects isolate within-unit variation over time  
- Evaluated the relationship between explanatory variables and the dependent variable while controlling for unit-level effects  

---

## How to Use
1. Clone or download this repository  
2. Open the relevant R script (`.R` file)  
3. Run the script to reproduce the analysis and results  

---

✨ This portfolio demonstrates practical skills in econometrics, time series forecasting, and panel data modeling using **R**.
