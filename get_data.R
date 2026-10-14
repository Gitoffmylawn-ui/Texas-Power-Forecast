# Step 1 of 2: download and clean the training data.
#   - ERCOT hourly load by weather zone   https://www.ercot.com/gridinfo/load/load_hist
#   - Open-Meteo historical hourly weather https://open-meteo.com/en/docs/historical-weather-api
# Output: data/ercot_hourly_load.csv, data/weather_hourly.csv, data/zone_weights.csv
#
# Re-run it to refresh the data (e.g. monthly). Files already downloaded are reused,
# except the current year's ERCOT file and weather, which are always re-downloaded.

library(readxl)
library(jsonlite)
source("features.R")

dir.create("data/ercot_raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/weather_raw", recursive = TRUE, showWarnings = FALSE)
options(timeout = 300)

# ---- 1. ERCOT load ----------------------------------------------------
# ERCOT's file names/paths change every year. When a new year appears on the
# load_hist page, add its link here.
ercot_urls <- c(
  "2016" = "https://www.ercot.com/files/docs/2017/01/10/native_Load_2016.zip",
  "2017" = "https://www.ercot.com/files/docs/2018/01/09/native_load_2017.zip",
  "2018" = "https://www.ercot.com/files/docs/2019/01/07/native_load_2018.zip",
  "2019" = "https://www.ercot.com/files/docs/2020/01/09/Native_Load_2019.zip",
  "2020" = "https://www.ercot.com/files/docs/2021/01/12/Native_Load_2020.zip",
  "2021" = "https://www.ercot.com/files/docs/2021/11/12/Native_Load_2021.zip",
  "2022" = "https://www.ercot.com/files/docs/2022/02/08/Native_Load_2022.zip",
  "2023" = "https://www.ercot.com/files/docs/2023/02/09/Native_Load_2023.zip",
  "2024" = "https://www.ercot.com/files/docs/2024/02/06/Native_Load_2024.zip",
  "2025" = "https://www.ercot.com/files/docs/2025/02/11/Native_Load_2025.zip",
  "2026" = "https://www.ercot.com/files/docs/2026/02/10/Native_Load_2026-1-.zip"
)
this_year <- format(Sys.Date(), "%Y")

read_ercot_year <- function(year) {
  zip <- file.path("data/ercot_raw", paste0("load_", year, ".zip"))
  dir <- file.path("data/ercot_raw", year)
  if (!file.exists(zip) || year == this_year) {
    download.file(ercot_urls[[year]], zip, mode = "wb", quiet = TRUE)
    unlink(dir, recursive = TRUE)
  }
  if (!dir.exists(dir)) unzip(zip, exdir = dir)
  f <- list.files(dir, pattern = "\\.xlsx?$", full.names = TRUE)[1]
  d <- as.data.frame(read_excel(f))
  if (ncol(d) != 10) stop("Unexpected ERCOT layout in ", f)
  names(d) <- c("HourEnd", "COAST", "EAST", "FWEST", "NORTH", "NCENT", "SOUTH", "SCENT", "WEST", "ERCOT")

  # The timestamp column is "hour ending" and comes in different formats by year
  he <- d$HourEnd
  if (is.numeric(he)) he <- as.POSIXct((he - 25569) * 86400, origin = "1970-01-01", tz = "UTC")
  if (inherits(he, "POSIXct")) {
    # e.g. 2016-01-01 01:59:59 -> round to the hour, then step back to the hour beginning
    start <- as.POSIXct(round(as.numeric(he) / 3600) * 3600 - 3600, origin = "1970-01-01", tz = "UTC")
    d$Date <- as.Date(start)
    d$Hour <- as.integer(format(start, "%H", tz = "UTC"))
  } else {
    # e.g. "01/01/2021 24:00" or "11/06/2022 02:00DST"
    he <- trimws(he)
    d$Date <- as.Date(substr(he, 1, 10), format = "%m/%d/%Y")
    d$Hour <- as.integer(substr(he, 12, 13)) - 1
  }
  d$HourEnd <- NULL
  d[!is.na(d$Date) & !is.na(d$ERCOT), ]
}

cat("Reading ERCOT load files...\n")
load <- do.call(rbind, lapply(names(ercot_urls), read_ercot_year))
# Daylight-saving fall-back repeats an hour: average the duplicates
load <- aggregate(load[region_codes], by = load[c("Date", "Hour")], FUN = mean)
load <- load[order(load$Date, load$Hour), ]
write.csv(load, "data/ercot_hourly_load.csv", row.names = FALSE)
cat(sprintf("  %d hours of load, %s to %s\n", nrow(load), min(load$Date), max(load$Date)))

# Each zone's share of statewide load over the last year, used to weight statewide weather
recent <- load[load$Date > max(load$Date) - 365, ]
zone_weights <- colSums(recent[names(zones)]) / sum(colSums(recent[names(zones)]))
write.csv(data.frame(Region = names(zone_weights), Weight = zone_weights), "data/zone_weights.csv", row.names = FALSE)

# ---- 2. Historical weather -------------------------------------------

get_city_history <- function(i, start, end) {
  f <- file.path("data/weather_raw", paste0(gsub(" ", "_", cities$city[i]), ".csv"))
  if (file.exists(f)) {
    old <- read.csv(f)
    if (max(as.Date(old$Date)) >= end) return(old)
  }
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive",
    "?latitude=", cities$lat[i], "&longitude=", cities$lon[i],
    "&start_date=", start, "&end_date=", end,
    "&hourly=", paste(weather_vars, collapse = ","),
    "&temperature_unit=fahrenheit&wind_speed_unit=mph&timezone=America%2FChicago"
  )
  h <- fromJSON(url)$hourly
  out <- data.frame(city = cities$city[i], Date = substr(h$time, 1, 10),
                    Hour = as.integer(substr(h$time, 12, 13)))
  for (v in names(weather_vars)) out[[v]] <- h[[weather_vars[[v]]]]
  write.csv(out, f, row.names = FALSE)
  Sys.sleep(40)   # a 10-year request counts as many API calls; stay under the per-minute limit
  out
}

cat("Downloading historical weather (about 40 seconds per city)...\n")
city_wx <- do.call(rbind, lapply(seq_len(nrow(cities)), function(i) {
  cat("  ", cities$city[i], "\n")
  get_city_history(i, min(load$Date), max(load$Date))
}))
city_wx$Date <- as.Date(city_wx$Date)
city_wx <- aggregate(city_wx[names(weather_vars)], by = city_wx[c("city", "Date", "Hour")], FUN = mean)

cat("Building zone and statewide weather...\n")
wx <- region_weather(city_wx, zone_weights)
write.csv(wx, "data/weather_hourly.csv", row.names = FALSE)
cat("Done. Now run train_model.R\n")
