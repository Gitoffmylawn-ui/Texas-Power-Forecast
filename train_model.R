# Step 2 of 2: compare models, then train the final ones and save them for the app.
# Needs the files written by get_data.R. Output: power_models.rds, data/model_accuracy.csv
#
# Models
#   Daily  (one per region): ARIMAX = regression with ARIMA errors on cooling/heating
#          degree days, dew point, wind, cloud cover, holidays, weekday and yearly
#          seasonality. This is what the app's daily numbers come from.
#   Hourly (one per region): regression on hour of day x weekday, hourly weather and
#          holidays. The app uses its shape to split each day's ARIMAX total into hours.
#
# Testing: the last 52 weeks are held out. Starting each week, every method forecasts
# the next 7 days (what the app does) without seeing those days, using actual weather.

library(forecast)
source("features.R")

test_weeks <- 52
test_days  <- test_weeks * 7

load <- read.csv("data/ercot_hourly_load.csv"); load$Date <- as.Date(load$Date)
wx   <- read.csv("data/weather_hourly.csv");    wx$Date   <- as.Date(wx$Date)
zw   <- read.csv("data/zone_weights.csv")
zone_weights <- setNames(zw$Weight, zw$Region)

# ---- Data for one region ----

build_region <- function(code) {
  lh <- load[c("Date", "Hour", code)]
  names(lh)[3] <- "Load"                                   # MW (= MWh in that hour)
  hourly <- merge(lh, wx[wx$Region == code, c("Date", "Hour", names(weather_vars))], by = c("Date", "Hour"))
  hourly <- hourly[complete.cases(hourly), ]

  n_hours <- aggregate(Load ~ Date, hourly, length)
  daily <- aggregate(Load ~ Date, hourly, sum)
  daily$Load <- daily$Load / 1000                          # GWh per day
  daily <- daily[daily$Date %in% n_hours$Date[n_hours$Load >= 23], ]
  daily <- merge(daily, daily_weather(hourly), by = "Date")
  daily <- merge(daily, calendar_features(daily$Date), by = "Date")
  daily <- daily[order(daily$Date), ]

  gaps <- sum(diff(daily$Date) != 1)
  if (gaps > 0) warning(code, ": ", gaps, " gaps in the daily series")
  list(hourly = hourly, daily = daily)
}

# ---- Testing helpers ----

metrics <- function(actual, pred) {
  c(MAPE = mean(abs((actual - pred) / actual)) * 100,
    RMSE = sqrt(mean((actual - pred)^2)),
    MAE  = mean(abs(actual - pred)))
}

# Forecast 7 days ahead from each weekly origin with a model fitted on the training
# period (coefficients fixed; the model just sees the data up to each origin).
roll_arima <- function(fit, y, X, origins, freq) {
  as.vector(sapply(origins, function(o) {
    refit <- Arima(ts(y[1:o], frequency = freq), xreg = X[1:o, , drop = FALSE], model = fit)
    as.numeric(forecast(refit, xreg = X[(o + 1):(o + 7), , drop = FALSE], h = 7)$mean)
  }))
}

fit_hourly <- function(h) {
  m <- lm(hourly_formula, data = h, model = FALSE, qr = FALSE)
  tt <- terms(m)
  attr(tt, ".Environment") <- globalenv()
  list(coef = coef(m), terms = tt, xlevels = m$xlevels, contrasts = m$contrasts)
}

refit_full <- function(fit, y, X) {
  Arima(ts(y), order = arimaorder(fit)[1:3], xreg = X,
        include.mean  = "intercept" %in% names(coef(fit)),
        include.drift = "drift" %in% names(coef(fit)))
}

# ---- Run ----

accuracy <- list()
bundle <- list(regions = list(), zone_weights = zone_weights)

for (code in region_codes) {
  cat("\n====", region_labels[[code]], "====\n")
  r <- build_region(code)
  daily <- r$daily
  y <- daily$Load
  n <- nrow(daily)
  n_train <- n - test_days
  origins <- n_train + seq(0, by = 7, length.out = test_weeks)
  actual  <- as.vector(sapply(origins, function(o) y[(o + 1):(o + 7)]))
  X_new <- daily_xreg(daily)

  cat("Fitting upgraded daily ARIMAX...\n")
  fit_new <- auto.arima(ts(y[1:n_train]), xreg = X_new[1:n_train, ], seasonal = FALSE)
  print(fit_new)
  preds <- list("ARIMAX (upgraded)" = roll_arima(fit_new, y, X_new, origins, 1))

  if (code == "ERCOT") {
    cat("Fitting comparison models on the same data...\n")
    X_old <- original_xreg(daily)
    fit_old <- auto.arima(ts(y[1:n_train], frequency = 7), xreg = X_old[1:n_train, ], seasonal = TRUE)
    preds[["ARIMAX (original 2016 setup)"]] <- roll_arima(fit_old, y, X_old, origins, 7)

    fit_ets <- ets(ts(y[1:n_train], frequency = 7))
    preds[["ETS"]] <- as.vector(sapply(origins, function(o) as.numeric(
      forecast(ets(ts(y[1:o], frequency = 7), model = fit_ets, use.initial.values = TRUE), h = 7)$mean)))

    preds[["Seasonal naive"]] <- as.vector(sapply(origins, function(o) y[(o - 6):o]))
  }

  for (m in names(preds)) {
    accuracy[[length(accuracy) + 1]] <- data.frame(
      Region = code, Level = "Daily", Method = m, t(metrics(actual, preds[[m]])), check.names = FALSE)
  }

  # Hourly model: train on the training period, test on the held-out year
  cat("Fitting hourly model...\n")
  hf <- hourly_features(r$hourly)
  train_end <- daily$Date[n_train]
  test_rows <- hf$Date > train_end & hf$Date <= daily$Date[max(origins) + 7]
  hm_train <- fit_hourly(hf[hf$Date <= train_end, ])
  hpred <- predict_hourly(hm_train, hf[test_rows, ])
  accuracy[[length(accuracy) + 1]] <- data.frame(
    Region = code, Level = "Hourly", Method = "Hourly regression",
    t(metrics(hf$Load[test_rows], hpred)), check.names = FALSE)

  # Final models on all data
  cat("Refitting on all data...\n")
  bundle$regions[[code]] <- list(
    daily      = refit_full(fit_new, y, X_new),
    hourly     = fit_hourly(hf),
    temp_range = range(r$hourly$Temp)
  )
  bundle$trained_through <- max(daily$Date)
  rm(r, hf); gc(verbose = FALSE)
}

accuracy <- do.call(rbind, accuracy)
accuracy$MAPE <- round(accuracy$MAPE, 2)
accuracy$RMSE <- round(accuracy$RMSE, 1)
accuracy$MAE  <- round(accuracy$MAE, 1)
rownames(accuracy) <- NULL
bundle$accuracy <- accuracy
bundle$test_period <- c(bundle$trained_through - test_days + 1, bundle$trained_through)

write.csv(accuracy, "data/model_accuracy.csv", row.names = FALSE)
saveRDS(bundle, "power_models.rds")

cat("\nAccuracy on the held-out year (daily in GWh, hourly in MW):\n")
print(accuracy)
cat("\nSaved power_models.rds (trained through", format(bundle$trained_through), ")\n")
