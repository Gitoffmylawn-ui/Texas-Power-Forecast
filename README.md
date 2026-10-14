# Texas Power Usage — 7-Day Forecast

An interactive R Shiny app that forecasts Texas electricity demand for the next 7 days from the live weather forecast, statewide and for each of ERCOT's 8 weather zones, down to the hour.

**Live app:** https://bonnyandclaude.shinyapps.io/texas-power-forecast/


## What it does

- Pulls the latest hourly weather forecast for 11 Texas cities from Open-Meteo
- Predicts daily electricity use (GWh) with a regression-with-ARIMA-errors (ARIMAX) model
- Breaks each day into hourly demand (GW) with an hourly load model, showing the morning low and afternoon peak
- Lets you switch between statewide demand and any of ERCOT's 8 weather zones (Houston, Dallas–Fort Worth, Austin–San Antonio, etc.)

## Data

| Source | What | Period |
|---|---|---|
| [ERCOT hourly load archives](https://www.ercot.com/gridinfo/load/load_hist) | Hourly electricity demand by weather zone (MW) | Jan 2016 – Sep 2026 (~94,000 hours) |
| [Open-Meteo historical weather API](https://open-meteo.com/en/docs/historical-weather-api) | Hourly temperature, dew point, humidity, wind, cloud cover for 11 cities | Same period |
| [Open-Meteo forecast API](https://open-meteo.com/en/docs) | 7-day hourly forecast used by the live app | Live |

Statewide weather is a demand-weighted average of the zones (Houston and Dallas–Fort Worth make up over half of ERCOT demand).

## Method

**Daily model: ARIMAX (regression with ARIMA errors), one per region.** Regressors:

- Cooling and heating degree-hours (base 65°F) and their squares, which capture the steep rise in demand at temperature extremes
- Dew point, wind speed and cloud cover
- Day of week and holidays
- Yearly seasonality (Fourier terms)
- Indicator variables for one-off events: the COVID-19 lockdown (spring 2020) and Winter Storm Uri (Feb 2021, when forced outages pushed recorded demand far below actual need)

**Hourly model: linear regression, one per region,** based on Tao Hong's "vanilla" load-forecasting benchmark: hour of day × weekday, hour × temperature (cubic), month × temperature, plus dew point, wind, cloud cover and holidays. In the app, the hourly model's shape is scaled so the 24 hours add up to the daily ARIMAX forecast.

**Evaluation.** The last 52 weeks (Oct 2025 – Sep 2026) were held out. Each week, every method forecast the next 7 days without seeing them (using the actual weather), the same task the app performs.

## Results (statewide, daily, held-out year)

| Method | MAPE | MAE (GWh/day) | RMSE (GWh/day) |
|---|---|---|---|
| **ARIMAX (upgraded)** | **1.88%** | **24.9** | **30.8** |
| ARIMAX (original specification: temperature, temperature², humidity, wind) | 2.26% | 30.3 | 39.2 |
| ETS (exponential smoothing) | 5.41% | 73.7 | 105.7 |
| Seasonal naive | 6.58% | 88.4 | 116.3 |

Daily MAPE for the individual zones ranged from 2.1% (Coast/Houston) to 4.2% (North/Wichita Falls). The full table, including hourly accuracy, is in [`data/model_accuracy.csv`](data/model_accuracy.csv).

This project started as a single-year (2016) analysis comparing seasonal naive, ETS and ARIMAX; ARIMAX with weather regressors was the best of the three. The upgrade added ten more years of data, zone-level demand, better weather features and calendar effects, cutting the ARIMAX error by about 17%.

## Run it yourself

Requires R with: `shiny`, `bslib`, `forecast`, `jsonlite`, `ggplot2`, `readxl`

```r
# The app (uses the trained models in power_models.rds)
shiny::runApp()

# Refresh the data and retrain (about 10 minutes each)
source("get_data.R")
source("train_model.R")
```

## Files

| File | Purpose |
|---|---|
| `app.R` | The Shiny app |
| `features.R` | Shared feature definitions (zones, calendar, weather features), used by training and the app |
| `get_data.R` | Downloads and cleans ERCOT load and historical weather |
| `train_model.R` | Compares methods on the held-out year and trains the final models |
| `power_models.rds` | Trained models used by the app |
| `deploy.R` | Publishes the app to shinyapps.io |

## Credits

- Electricity demand data: [ERCOT](https://www.ercot.com/)
- Weather data: [Open-Meteo.com](https://open-meteo.com/) (CC BY 4.0)
- Header photo: Santa Elena Canyon, Big Bend National Park, by G. Lamar, [CC BY 2.0](https://creativecommons.org/licenses/by/2.0/), via [Wikimedia Commons](https://commons.wikimedia.org/wiki/File:Big_Bend_National_Park-_Santa_Elena_Canyon_Entrance_(52093111867).jpg)
