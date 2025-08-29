# NVDA Time Series Forecasting (ARMA)

This project analyzes and forecasts NVIDIA (NVDA) stock prices using classical time series methods in **R**.  
The goal is to identify trends, select the optimal model, and evaluate forecasting accuracy.

## Methods
- Data exploration and visualization across multiple years
- Stationarity check (ADF test)
- Autocorrelation and Partial Autocorrelation analysis (ACF/PACF)
- ARMA model selection (based on AIC)
- Forecast evaluation with RMSE and MAE

## Files
- `NVDA_ARMA_Forecast.R` – R script containing the full workflow  
- `data_nvda.csv` – dataset with NVDA stock daily prices (source: Yahoo Finance)  
- `plots/` – saved figures of diagnostics and forecasts  

## Key Outcomes
- Identified changes and trends in NVDA stock across years  
- Selected the optimal ARMA model for forecasting  
- Generated forecasts and compared them to actual values with visual plots  

## How to Use
1. Clone or download this repository  
2. Open the R script `NVDA_ARMA_Forecast.R`  
3. Run the script to reproduce the analysis and forecasts  
