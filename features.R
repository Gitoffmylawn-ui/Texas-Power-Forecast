# Shared definitions used by get_data.R, train_model.R and app.R, so the model
# is trained and used with exactly the same inputs.

# ---- ERCOT weather zones and the cities used for each zone's weather ----

cities <- data.frame(
  city = c("Houston", "Tyler", "Midland", "Wichita Falls", "Dallas", "Fort Worth",
           "Corpus Christi", "McAllen", "Austin", "San Antonio", "Abilene"),
  lat  = c(29.7604, 32.3513, 31.9973, 33.9137, 32.7767, 32.7555,
           27.8006, 26.2034, 30.2672, 29.4241, 32.4487),
  lon  = c(-95.3698, -95.3011, -102.0779, -98.4934, -96.7970, -97.3308,
           -97.3964, -98.2300, -97.7431, -98.4936, -99.7331)
)

zones <- list(
  COAST = list(label = "Coast (Houston)",                              cities = "Houston"),
  NCENT = list(label = "North Central (Dallas–Fort Worth)",            cities = c("Dallas", "Fort Worth")),
  SCENT = list(label = "South Central (Austin–San Antonio)",           cities = c("Austin", "San Antonio")),
  SOUTH = list(label = "Southern (Corpus Christi–Rio Grande Valley)",  cities = c("Corpus Christi", "McAllen")),
  FWEST = list(label = "Far West (Midland–Odessa)",                    cities = "Midland"),
  WEST  = list(label = "West (Abilene)",                               cities = "Abilene"),
  NORTH = list(label = "North (Wichita Falls)",                        cities = "Wichita Falls"),
  EAST  = list(label = "East (Tyler)",                                 cities = "Tyler")
)
statewide_label <- "Texas — ERCOT statewide"
region_codes  <- c("ERCOT", names(zones))
region_labels <- setNames(c(statewide_label, sapply(zones, `[[`, "label")), region_codes)

# Hourly weather variables pulled from Open-Meteo (archive for training, forecast for the app)
weather_vars <- c(Temp = "temperature_2m", Dew = "dew_point_2m", Hum = "relative_humidity_2m",
                  Wind = "wind_speed_10m", Cloud = "cloud_cover")

# Average the cities of each zone, then build statewide weather as a load-weighted
# average of the zones. `city_wx` is long: city, Date, Hour, Temp, Dew, Hum, Wind, Cloud.
region_weather <- function(city_wx, zone_weights) {
  vars <- names(weather_vars)
  present <- names(zones)[sapply(zones, function(z) any(z$cities %in% city_wx$city))]
  by_zone <- lapply(present, function(z) {
    w <- city_wx[city_wx$city %in% zones[[z]]$cities, ]
    out <- aggregate(w[vars], by = w[c("Date", "Hour")], FUN = mean)
    out$Region <- z
    out
  })
  zw <- do.call(rbind, by_zone)
  zw$weight <- unname(zone_weights[zw$Region])
  wsum <- aggregate(cbind(zw[vars] * zw$weight, weight = zw$weight), by = zw[c("Date", "Hour")], FUN = sum)
  state <- data.frame(wsum[c("Date", "Hour")], wsum[vars] / wsum$weight, Region = "ERCOT")
  out <- rbind(zw[c("Date", "Hour", vars, "Region")], state)
  out[order(out$Region, out$Date, out$Hour), ]
}

# ---- Calendar ----

nth_weekday <- function(year, month, wday, n) {   # wday: 0 = Sunday ... 6 = Saturday; n = -1 for last
  days <- seq(as.Date(sprintf("%d-%02d-01", year, month)), by = "day", length.out = 31)
  days <- days[as.integer(format(days, "%m")) == month & as.POSIXlt(days)$wday == wday]
  if (n > 0) days[n] else days[length(days)]
}

texas_holidays <- function(years) {
  out <- lapply(years, function(y) c(
    as.Date(sprintf("%d-01-01", y)),     # New Year's Day
    nth_weekday(y, 1, 1, 3),             # MLK Day
    nth_weekday(y, 5, 1, -1),            # Memorial Day
    as.Date(sprintf("%d-07-04", y)),     # Independence Day
    nth_weekday(y, 9, 1, 1),             # Labor Day
    nth_weekday(y, 11, 4, 4),            # Thanksgiving
    nth_weekday(y, 11, 4, 4) + 1,        # Day after Thanksgiving
    as.Date(sprintf("%d-12-24", y)),     # Christmas Eve
    as.Date(sprintf("%d-12-25", y)),     # Christmas
    as.Date(sprintf("%d-12-31", y))      # New Year's Eve
  ))
  do.call(c, out)
}

dow_levels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

calendar_features <- function(dates) {
  doy <- as.numeric(format(dates, "%j"))
  wd  <- as.POSIXlt(dates)$wday
  data.frame(
    Date    = dates,
    DOW     = factor(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[wd + 1], levels = dow_levels),
    Month   = factor(as.integer(format(dates, "%m")), levels = 1:12),
    Holiday = as.integer(dates %in% texas_holidays(unique(as.integer(format(dates, "%Y"))))),
    # One-off events that would otherwise distort the model (always 0 in forecasts)
    Uri     = as.integer(dates >= as.Date("2021-02-14") & dates <= as.Date("2021-02-20")),
    Covid   = as.integer(dates >= as.Date("2020-03-20") & dates <= as.Date("2020-05-15")),
    Trend   = as.numeric(dates - as.Date("2016-01-01")) / 365.25,
    # Yearly seasonality (Fourier terms)
    S1 = sin(2 * pi * doy / 365.25), C1 = cos(2 * pi * doy / 365.25),
    S2 = sin(4 * pi * doy / 365.25), C2 = cos(4 * pi * doy / 365.25),
    S3 = sin(6 * pi * doy / 365.25), C3 = cos(6 * pi * doy / 365.25)
  )
}

# ---- Daily model inputs ----

base_temp <- 65   # °F; above this people cool, below it they heat

# Hourly weather -> one row per day. Degree days are averaged over the 24 hours,
# so a hot afternoon counts even if the night was cool.
daily_weather <- function(hw) {
  hw$cd <- pmax(hw$Temp - base_temp, 0)
  hw$hd <- pmax(base_temp - hw$Temp, 0)
  m <- aggregate(hw[c("Temp", "Dew", "Hum", "Wind", "Cloud", "cd", "hd")], by = hw["Date"], FUN = mean)
  names(m) <- c("Date", "Temp_avg", "Dew_avg", "Hum_avg", "Wind_avg", "Cloud_avg", "CDD", "HDD")
  m$Temp_max <- aggregate(hw$Temp, by = hw["Date"], FUN = max)$x
  m$Temp_min <- aggregate(hw$Temp, by = hw["Date"], FUN = min)$x
  m
}

# Regressors for the upgraded daily ARIMAX
daily_xreg <- function(d) {
  X <- model.matrix(~ CDD + I(CDD^2) + HDD + I(HDD^2) + Dew_avg + Wind_avg + Cloud_avg +
                      Holiday + DOW + S1 + C1 + S2 + C2 + S3 + C3 + Uri + Covid, data = d)
  X[, colnames(X) != "(Intercept)", drop = FALSE]
}

# Regressors of the original 2016 model, kept for comparison
original_xreg <- function(d) {
  cbind(Temp_avg = d$Temp_avg, Temp_avg2 = d$Temp_avg^2, Hum_avg = d$Hum_avg, Wind_avg = d$Wind_avg)
}

# ---- Hourly model inputs ----

hourly_features <- function(hw) {
  cal <- calendar_features(sort(unique(hw$Date)))
  h <- merge(hw, cal, by = "Date")
  h$HourF <- factor(h$Hour, levels = 0:23)
  h$T  <- (h$Temp - base_temp) / 10
  h$T2 <- h$T^2
  h$T3 <- h$T^3
  h$DewS <- (h$Dew - 55) / 10
  h[order(h$Date, h$Hour), ]
}

# Based on Tao Hong's "vanilla" load-forecasting benchmark, plus humidity,
# wind, cloud cover, holidays and the one-off events.
hourly_formula <- Load ~ Trend + Month + DOW:HourF + Holiday:HourF +
  Month:(T + T2 + T3) + HourF:(T + T2 + T3) + HourF:DewS + Wind + Cloud + Uri + Covid

# Predict from a slimmed-down lm (coefficients + terms only, so the saved file stays small)
predict_hourly <- function(m, newdata) {
  X <- model.matrix(delete.response(m$terms), newdata, xlev = m$xlevels, contrasts.arg = m$contrasts)
  b <- m$coef[colnames(X)]
  b[is.na(b)] <- 0
  as.numeric(X %*% b)
}
