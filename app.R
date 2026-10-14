# Texas Power Usage - 7-Day Forecast
# Uses the models saved by train_model.R (power_models.rds) and live weather
# forecasts from Open-Meteo (free, no API key): https://open-meteo.com
#
# Needs in the same folder: app.R, features.R, power_models.rds
# Run with:  shiny::runApp("path/to/folder")

library(shiny)
library(bslib)
library(forecast)
library(jsonlite)
library(ggplot2)
source("features.R")

# ---- Load the models ------------------------------------------------

bundle <- readRDS("power_models.rds")
tz <- "America/Chicago"
# "Today" in Texas, whatever timezone the server runs in (shinyapps.io uses UTC)
today_tx <- function() as.Date(format(Sys.time(), "%Y-%m-%d", tz = tz))

# ---- Weather forecast -----------------------------------------------

# One request for all the cities needed (Open-Meteo accepts lists of coordinates)
get_city_forecasts <- function(idx, past_days) {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast",
    "?latitude=", paste(cities$lat[idx], collapse = ","),
    "&longitude=", paste(cities$lon[idx], collapse = ","),
    "&hourly=", paste(c(weather_vars, "weather_code", "precipitation_probability", "is_day"), collapse = ","),
    "&daily=weather_code,precipitation_probability_max",
    "&temperature_unit=fahrenheit&wind_speed_unit=mph&timezone=America%2FChicago",
    "&forecast_days=7&past_days=", past_days
  )
  res <- fromJSON(url, simplifyVector = FALSE)
  if (!is.null(res$hourly)) res <- list(res)       # a single location comes back unwrapped
  num <- function(x) vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))

  lapply(seq_along(idx), function(k) {
    h <- res[[k]]$hourly
    d <- res[[k]]$daily
    time <- unlist(h$time)
    hourly <- data.frame(city = cities$city[idx[k]], Date = as.Date(substr(time, 1, 10)),
                         Hour = as.integer(substr(time, 12, 13)))
    for (v in names(weather_vars)) hourly[[v]] <- num(h[[weather_vars[[v]]]])
    hourly$Code     <- num(h$weather_code)
    hourly$Rain_pct <- num(h$precipitation_probability)
    hourly$Is_day   <- num(h$is_day)
    daily <- data.frame(city = cities$city[idx[k]], Date = as.Date(unlist(d$time)),
                        Code = num(d$weather_code), Rain_pct = num(d$precipitation_probability_max))
    list(hourly = hourly, daily = daily)
  })
}

# Weather for a region: the same zone/statewide averaging used in training,
# plus icons and rain chance for display (worst weather / average rain chance).
get_region_weather <- function(code) {
  # Fetch the days between the end of the training data and today too, so the
  # model can be rolled forward through them (Open-Meteo keeps up to 92 past days).
  gap <- as.integer(today_tx() - bundle$trained_through) - 1
  past_days <- max(0, min(gap, 92))
  use <- if (code == "ERCOT") cities$city else zones[[code]]$cities
  fc <- get_city_forecasts(which(cities$city %in% use), past_days)

  city_hourly <- do.call(rbind, lapply(fc, `[[`, "hourly"))
  city_hourly <- city_hourly[complete.cases(city_hourly[names(weather_vars)]), ]
  wx <- region_weather(city_hourly, bundle$zone_weights)
  wx <- wx[wx$Region == code, c("Date", "Hour", names(weather_vars))]

  look <- aggregate(cbind(Code = city_hourly$Code, Rain_pct = city_hourly$Rain_pct, Is_day = city_hourly$Is_day),
                    by = city_hourly[c("Date", "Hour")],
                    FUN = function(x) c(max(x, na.rm = TRUE), mean(x, na.rm = TRUE)))
  hourly <- merge(wx, data.frame(look[c("Date", "Hour")],
                                 Code = look$Code[, 1], Rain_pct = look$Rain_pct[, 2], Is_day = look$Is_day[, 1]),
                  by = c("Date", "Hour"))
  hourly$Time <- as.POSIXct(sprintf("%s %02d:00", hourly$Date, hourly$Hour), tz = tz)
  hourly <- hourly[order(hourly$Date, hourly$Hour), ]

  cd <- do.call(rbind, lapply(fc, `[[`, "daily"))
  daily_look <- aggregate(cbind(Code = cd$Code, Rain_pct = cd$Rain_pct), by = cd["Date"],
                          FUN = function(x) c(max(x, na.rm = TRUE), mean(x, na.rm = TRUE)))
  daily_look <- data.frame(Date = daily_look$Date, Code = daily_look$Code[, 1], Rain_pct = daily_look$Rain_pct[, 2])
  forecast_dates <- daily_look$Date[daily_look$Date >= today_tx()][1:7]

  list(hourly = hourly, daily_look = daily_look, forecast_dates = forecast_dates[!is.na(forecast_dates)])
}

# Weather is shared by everyone using the app and kept for 30 minutes, so only the
# first visitor in that window waits for Open-Meteo (forecasts update hourly anyway).
weather_cache <- new.env()
cached_region_weather <- function(code, force = FALSE) {
  hit <- weather_cache[[code]]
  fresh <- !is.null(hit) && difftime(Sys.time(), hit$fetched, units = "mins") < 30 &&
           format(hit$fetched, "%Y-%m-%d", tz = tz) == format(today_tx())
  if (fresh && !force) return(hit)
  w <- get_region_weather(code)
  w$fetched <- Sys.time()
  assign(code, w, envir = weather_cache)
  w
}

# WMO weather code -> label, Font Awesome icon, icon color
weather_look <- function(code, is_day = 1) {
  night <- !is.na(is_day) && is_day == 0
  if (is.na(code) || !is.finite(code)) return(list(label = "Unknown", icon = "question", color = "#95a5a6"))
  if (code == 0)    return(if (night) list(label = "Clear", icon = "moon", color = "#7f8c8d")
                           else list(label = "Sunny", icon = "sun", color = "#f39c12"))
  if (code <= 2)    return(if (night) list(label = "Partly cloudy", icon = "cloud-moon", color = "#7f8c8d")
                           else list(label = "Partly cloudy", icon = "cloud-sun", color = "#f1a92b"))
  if (code == 3)    return(list(label = "Cloudy",        icon = "cloud",               color = "#7f8c8d"))
  if (code <= 48)   return(list(label = "Fog",           icon = "smog",                color = "#95a5a6"))
  if (code <= 57)   return(list(label = "Drizzle",       icon = "cloud-rain",          color = "#3498db"))
  if (code <= 67)   return(list(label = "Rain",          icon = "cloud-showers-heavy", color = "#2980b9"))
  if (code <= 77)   return(list(label = "Snow",          icon = "snowflake",           color = "#5dade2"))
  if (code <= 82)   return(list(label = "Showers",       icon = "cloud-rain",          color = "#2980b9"))
  if (code <= 86)   return(list(label = "Snow showers",  icon = "snowflake",           color = "#5dade2"))
  list(label = "Thunderstorms", icon = "cloud-bolt", color = "#8e44ad")
}

# ---- Prediction -----------------------------------------------------

predict_region <- function(code, w) {
  m <- bundle$regions[[code]]

  # Daily: roll the ARIMAX forward from the day after training through the forecast week
  dw <- daily_weather(w$hourly)
  all_dates <- seq(bundle$trained_through + 1, max(w$forecast_dates), by = "day")
  dw <- merge(data.frame(Date = all_dates), dw, by = "Date", all.x = TRUE)
  # Days older than Open-Meteo's 92-day history: reuse the earliest weather available
  first_ok <- which(complete.cases(dw))[1]
  for (j in which(!complete.cases(dw))) dw[j, -1] <- dw[first_ok, -1]
  d <- merge(dw, calendar_features(all_dates), by = "Date")
  d <- d[order(d$Date), ]

  fc  <- forecast(m$daily, xreg = daily_xreg(d), h = nrow(d))
  idx <- match(w$forecast_dates, d$Date)
  daily <- data.frame(
    Date     = w$forecast_dates,
    Forecast = as.numeric(fc$mean)[idx],
    Lo80     = as.numeric(fc$lower[idx, 1]),
    Hi80     = as.numeric(fc$upper[idx, 1]),
    Lo95     = as.numeric(fc$lower[idx, 2]),
    Hi95     = as.numeric(fc$upper[idx, 2])
  )
  daily <- merge(daily, d[c("Date", "Temp_avg", "Temp_max", "Temp_min", "Dew_avg", "Wind_avg", "Cloud_avg", "Holiday")], by = "Date")
  daily <- merge(daily, w$daily_look, by = "Date")

  # Hourly: the hourly model's shape, scaled so each day's hours add up to the daily forecast
  hr <- w$hourly[w$hourly$Date %in% w$forecast_dates, ]
  hr$Shape <- pmax(predict_hourly(m$hourly, hourly_features(hr)), 1)
  hr <- hr[order(hr$Date, hr$Hour), ]
  # Daily energy (GWh) spread over the hours gives the average demand in each hour (GW)
  day_total <- daily$Forecast[match(hr$Date, daily$Date)]
  hr$Power  <- hr$Shape * day_total / ave(hr$Shape, hr$Date, FUN = sum)

  peaks <- do.call(rbind, lapply(split(hr, hr$Date), function(g) g[which.max(g$Power), c("Date", "Time", "Power")]))
  names(peaks) <- c("Date", "Peak_time", "Peak_GW")
  daily <- merge(daily, peaks, by = "Date")
  list(daily = daily[order(daily$Date), ], hourly = hr)
}

out_of_range <- function(code, hourly) {
  r <- bundle$regions[[code]]$temp_range
  if (any(hourly$Temp < r[1] | hourly$Temp > r[2]))
    sprintf("forecast temperatures fall outside anything in the training data (%.0f°F to %.0f°F)", r[1], r[2])
}

fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits, big.mark = ",")
hour_label <- function(t) sub("^0", "", format(t, "%I %p"))

accuracy_text <- function(code) {
  a <- bundle$accuracy
  mape <- function(method) a$MAPE[a$Region == code & a$Method == method & a$Level == "Daily"]
  new <- mape("ARIMAX (upgraded)")
  txt <- sprintf("Tested on the last 52 weeks of real data: 7-day daily forecasts were off by %.1f%% on average.", new)
  if (code == "ERCOT") {
    txt <- paste(txt, sprintf("The original 2016 setup was off by %.1f%% on the same test.", mape("ARIMAX (original 2016 setup)")))
  }
  txt
}

# ---- Styles ---------------------------------------------------------

# Header photo: Santa Elena Canyon, Big Bend National Park, by G. Lamar (CC BY 2.0),
# via Wikimedia Commons. To use your own picture, put it in a folder named "www"
# next to app.R and set this to its file name, e.g. "header.jpg".
header_image <- "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Big_Bend_National_Park-_Santa_Elena_Canyon_Entrance_%2852093111867%29.jpg/1920px-Big_Bend_National_Park-_Santa_Elena_Canyon_Entrance_%2852093111867%29.jpg"

header_css <- sprintf("
.bslib-page-sidebar > .navbar {
  min-height: 150px; align-items: flex-end; padding: 18px 24px;
  background: linear-gradient(90deg, rgba(0,0,0,.65) 0%%, rgba(0,0,0,.25) 60%%, rgba(0,0,0,.05) 100%%),
              url('%s') center 55%% / cover no-repeat, #7b4a2a;
  border-bottom: 4px solid #bf5700;
}
.bslib-page-sidebar > .navbar .navbar-brand { white-space: normal; }
.app-title    { color: #fff; font-size: 2rem; font-weight: 800; letter-spacing: .5px;
                text-shadow: 0 2px 8px rgba(0,0,0,.6); line-height: 1.1; }
.app-title .fa-star { color: #f5c542; margin-right: 8px; }
.app-subtitle { color: rgba(255,255,255,.9); font-size: 1rem; font-weight: 400; margin-top: 4px;
                text-shadow: 0 1px 4px rgba(0,0,0,.6); }
.photo-credit { font-size: .7rem; color: var(--bs-secondary-color); }
.photo-credit a { color: inherit; }
", header_image)

app_css <- "
/* Summary boxes */
.vb-navy   { background: linear-gradient(135deg, #1b2f4b 0%, #2f5d8a 100%) !important; }
.vb-orange { background: linear-gradient(135deg, #a84a00 0%, #e8892b 100%) !important; }
.vb-red    { background: linear-gradient(135deg, #7d1a1a 0%, #cf3f2e 100%) !important; }
.vb-blue   { background: linear-gradient(135deg, #17607a 0%, #3a9fc2 100%) !important; }
.vb-navy, .vb-orange, .vb-red, .vb-blue {
  color: #fff !important; border: none !important; border-radius: 14px !important;
  box-shadow: 0 6px 18px rgba(0,0,0,.12);
}
.vb-icon {
  width: 72px; height: 72px; border-radius: 50%;
  background: rgba(255,255,255,.16); box-shadow: inset 0 0 0 2px rgba(255,255,255,.25);
  display: flex; align-items: center; justify-content: center;
}
.vb-icon i { font-size: 2rem !important; color: #fff; }
.vb-navy .vb-icon i { color: #f5c542; }
.bslib-value-box .value-box-title { text-transform: uppercase; letter-spacing: .07em;
                                    font-size: .78rem; font-weight: 700; opacity: .85; }
.bslib-value-box .value-box-value { font-weight: 700; }
.vb-unit { font-size: .45em; font-weight: 600; opacity: .85; margin-left: 6px; }
.vb-sub  { font-size: .9rem; opacity: .9; }

.week-strip { display: grid; grid-template-columns: repeat(auto-fit, minmax(115px, 1fr)); gap: 10px; }
.day-tile { border: 1px solid var(--bs-border-color); border-radius: 10px; padding: 12px 8px;
            text-align: center; background: var(--bs-body-bg); cursor: pointer;
            transition: box-shadow .15s, transform .15s; }
.day-tile:hover { transform: translateY(-2px); box-shadow: 0 4px 12px rgba(0,0,0,.08); }
.day-tile.peak { border-top: 4px solid #e74c3c; }
.day-tile.low  { border-top: 4px solid #3498db; }
.day-name  { font-weight: 700; font-size: 1.05rem; }
.day-date  { color: var(--bs-secondary-color); font-size: .8rem; }
.holiday   { display: inline-block; font-size: .65rem; background: #bf5700; color: #fff;
             border-radius: 8px; padding: 0 6px; margin-top: 2px; }
.wx-icon   { font-size: 2.4rem; margin: 10px 0 4px; }
.wx-label  { font-size: .8rem; color: var(--bs-secondary-color); min-height: 1.2em; }
.wx-temps  { margin-top: 6px; font-size: 1.1rem; }
.wx-hi     { font-weight: 700; }
.wx-lo     { color: var(--bs-secondary-color); margin-left: 4px; }
.wx-rain   { font-size: .8rem; color: #2980b9; margin-top: 2px; min-height: 1.2em; }
.pw-block  { border-top: 1px dashed var(--bs-border-color); margin-top: 10px; padding-top: 10px; }
.pw-value  { font-weight: 700; font-size: 1.25rem; }
.pw-value .fa-bolt { color: #f1c40f; margin-right: 3px; }
.pw-unit   { font-size: .75rem; color: var(--bs-secondary-color); }
.pw-bar    { height: 6px; border-radius: 3px; background: var(--bs-tertiary-bg); margin-top: 6px; overflow: hidden; }
.pw-fill   { height: 100%; border-radius: 3px; background: linear-gradient(90deg, #3498db, #e74c3c); }
.pw-peak   { font-size: .72rem; color: var(--bs-secondary-color); margin-top: 4px; }

.hour-strip { display: flex; gap: 6px; overflow-x: auto; padding-bottom: 6px; }
.hour-tile  { flex: 0 0 76px; border: 1px solid var(--bs-border-color); border-radius: 8px;
              padding: 8px 4px; text-align: center; font-size: .85rem; }
.hour-tile.peak { border-top: 4px solid #e74c3c; }
.hour-tile.now  { background: var(--bs-tertiary-bg); }
.hour-time { font-weight: 600; }
.hour-icon { font-size: 1.4rem; margin: 6px 0 2px; }
.hour-temp { font-weight: 700; }
.hour-pw   { border-top: 1px dashed var(--bs-border-color); margin-top: 6px; padding-top: 6px; font-weight: 600; }
.hour-pw .fa-bolt { color: #f1c40f; }
"

# ---- Tiles ----------------------------------------------------------

# Icon in a round badge for the summary boxes
vb_icon <- function(name) div(class = "vb-icon", icon(name))

day_tile <- function(i, row, rel, classes) {
  look <- weather_look(row$Code)
  name <- if (row$Date == today_tx()) "Today" else format(row$Date, "%a")
  div(class = paste(c("day-tile", classes), collapse = " "),
    onclick = sprintf("Shiny.setInputValue('day_click', %d, {priority: 'event'})", i),
    div(class = "day-name", name),
    div(class = "day-date", format(row$Date, "%b %d")),
    if (row$Holiday == 1) div(class = "holiday", "Holiday"),
    div(class = "wx-icon", icon(look$icon, style = paste0("color:", look$color))),
    div(class = "wx-label", look$label),
    div(class = "wx-temps",
        span(class = "wx-hi", paste0(round(row$Temp_max), "°")),
        span(class = "wx-lo", paste0(round(row$Temp_min), "°"))),
    div(class = "wx-rain",
        if (!is.na(row$Rain_pct)) tagList(icon("droplet"), paste0(" ", round(row$Rain_pct), "%"))),
    div(class = "pw-block",
      div(class = "pw-value", icon("bolt"), fmt(row$Forecast)),
      div(class = "pw-unit", "GWh predicted"),
      div(class = "pw-bar", div(class = "pw-fill", style = sprintf("width:%.0f%%", 15 + 85 * rel))),
      div(class = "pw-peak", sprintf("Peak %s GW at %s", fmt(row$Peak_GW), hour_label(row$Peak_time)))
    )
  )
}

hour_tile <- function(row, classes) {
  look <- weather_look(row$Code, row$Is_day)
  div(class = paste(c("hour-tile", classes), collapse = " "),
    div(class = "hour-time", hour_label(row$Time)),
    div(class = "hour-icon", icon(look$icon, style = paste0("color:", look$color)), title = look$label),
    div(class = "hour-temp", paste0(round(row$Temp), "°")),
    div(class = "wx-rain",
        if (!is.na(row$Rain_pct) && row$Rain_pct > 0)
          tagList(icon("droplet"), paste0(" ", round(row$Rain_pct), "%"))),
    div(class = "hour-pw", icon("bolt"), fmt(row$Power)),
    div(class = "pw-unit", "GW")
  )
}

# ---- UI -------------------------------------------------------------

ui <- page_sidebar(
  title = div(style = "display: block;",
    div(class = "app-title", icon("star"), "Texas Power Usage"),
    div(class = "app-subtitle", "7-day electricity demand forecast driven by the weather")
  ),
  window_title = "Texas Power Usage — 7-Day Forecast",
  theme = bs_theme(bootswatch = "flatly"),
  fillable = FALSE,
  tags$head(tags$style(HTML(app_css)), tags$style(HTML(header_css))),
  sidebar = sidebar(
    selectInput("region", "Region", choices = setNames(region_codes, region_labels), selected = "ERCOT"),
    actionButton("refresh", "Refresh forecast", icon = icon("rotate"), class = "btn-primary"),
    hr(),
    helpText(sprintf("Model: ARIMAX trained on ERCOT hourly demand and Open-Meteo weather, 2016 to %s.",
                     format(bundle$trained_through, "%b %Y"))),
    helpText(textOutput("accuracy")),
    helpText("Weather data by ", a("Open-Meteo.com", href = "https://open-meteo.com/", target = "_blank"),
             " (CC BY 4.0). Demand data: ",
             a("ERCOT", href = "https://www.ercot.com/gridinfo/load/load_hist", target = "_blank"), "."),
    textOutput("updated"),
    div(class = "photo-credit",
        "Header photo: Santa Elena Canyon, Big Bend National Park, by G. Lamar, ",
        a("CC BY 2.0", href = "https://creativecommons.org/licenses/by/2.0/", target = "_blank"),
        ", via ",
        a("Wikimedia Commons", target = "_blank",
          href = "https://commons.wikimedia.org/wiki/File:Big_Bend_National_Park-_Santa_Elena_Canyon_Entrance_(52093111867).jpg"))
  ),
  uiOutput("warnings"),
  layout_columns(
    value_box("Total for the week",
              span(textOutput("total", inline = TRUE), span(class = "vb-unit", "GWh")),
              span(class = "vb-sub", "energy over the next 7 days"),
              showcase = vb_icon("bolt"), class = "vb-navy"),
    value_box("Peak day", textOutput("peak_day"),
              span(class = "vb-sub", textOutput("peak_val", inline = TRUE)),
              showcase = vb_icon("calendar-day"), class = "vb-orange"),
    value_box("Peak hourly demand", textOutput("peak_hour"),
              span(class = "vb-sub", textOutput("peak_hour_val", inline = TRUE)),
              showcase = vb_icon("gauge-high"), class = "vb-red")
  ),
  card(
    card_header(textOutput("week_title", inline = TRUE)),
    uiOutput("week_strip"),
    div(class = "small text-muted mt-2",
        "Click a day for its hour-by-hour breakdown. ",
        "The bar shows each day's usage relative to the rest of the week.")
  ),
  card(
    card_header("Daily detail"),
    tableOutput("detail")
  )
)

# ---- Server ---------------------------------------------------------

server <- function(input, output, session) {

  last_refresh <- 0
  results <- eventReactive(list(input$region, input$refresh), {
    force <- input$refresh > last_refresh       # the Refresh button skips the cache
    last_refresh <<- input$refresh
    withProgress(message = "Fetching weather forecast...", {
      w <- tryCatch(cached_region_weather(input$region, force), error = function(e) NULL)
      shiny::validate(need(!is.null(w), "Could not reach the Open-Meteo weather service. Check your internet connection and try Refresh."))
      incProgress(0.6, message = "Running the model...")
      p <- predict_region(input$region, w)
      c(p, list(code = input$region, time = w$fetched))
    })
  })

  output$updated  <- renderText(paste("Updated", format(results()$time, "%b %d, %I:%M %p", tz = tz), "(Texas time)"))
  output$accuracy <- renderText(accuracy_text(input$region))

  output$warnings <- renderUI({
    msg <- out_of_range(results()$code, results()$hourly)
    if (is.null(msg)) return(NULL)
    div(class = "alert alert-warning", strong("Heads up: "), "the model is extrapolating — ", msg, ".")
  })

  daily <- reactive(results()$daily)
  output$total <- renderText(fmt(sum(daily()$Forecast)))
  output$peak_day <- renderText(format(daily()$Date[which.max(daily()$Forecast)], "%a %b %d"))
  output$peak_val <- renderText(paste(fmt(max(daily()$Forecast)), "GWh"))
  output$peak_hour <- renderText({
    t <- daily()$Peak_time[which.max(daily()$Peak_GW)]
    paste(format(t, "%a"), hour_label(t))
  })
  output$peak_hour_val <- renderText(paste(fmt(max(daily()$Peak_GW)), "GW"))

  output$week_title <- renderText(paste("7-day forecast —", region_labels[[input$region]]))

  output$week_strip <- renderUI({
    d <- daily()
    span <- diff(range(d$Forecast))
    rel  <- if (span > 0) (d$Forecast - min(d$Forecast)) / span else rep(0.5, nrow(d))
    i_peak <- which.max(d$Forecast)
    i_low  <- which.min(d$Forecast)
    div(class = "week-strip",
      lapply(seq_len(nrow(d)), function(i) {
        classes <- if (i == i_peak) "peak" else if (i == i_low) "low"
        day_tile(i, d[i, ], rel[i], classes)
      })
    )
  })

  # ---- Pop-up: clicked day, hour by hour ----

  clicked <- reactiveVal(1)
  day_hours <- reactive({
    day <- daily()$Date[min(clicked(), nrow(daily()))]
    results()$hourly[results()$hourly$Date == day, ]
  })

  observeEvent(input$day_click, {
    clicked(input$day_click)
    d  <- daily()[input$day_click, ]
    hr <- day_hours()
    hi <- which.max(hr$Power)
    lo <- which.min(hr$Power)

    showModal(modalDialog(
      title = paste0(format(d$Date, "%A, %B %d"), " — hour-by-hour demand forecast"),
      size = "xl", easyClose = TRUE, footer = modalButton("Close"),
      layout_columns(
        value_box("Predicted for the day",
                  span(fmt(d$Forecast), span(class = "vb-unit", "GWh")),
                  span(class = "vb-sub", "energy used over 24 hours"),
                  showcase = vb_icon("bolt"), class = "vb-navy"),
        value_box("Peak hour", hour_label(hr$Time[hi]),
                  span(class = "vb-sub", paste(fmt(hr$Power[hi]), "GW demand")),
                  showcase = vb_icon("gauge-high"), class = "vb-red"),
        value_box("Lowest hour", hour_label(hr$Time[lo]),
                  span(class = "vb-sub", paste(fmt(hr$Power[lo]), "GW demand")),
                  showcase = vb_icon("arrow-trend-down"), class = "vb-blue")
      ),
      plotOutput("hour_plot", height = 340),
      uiOutput("hour_strip"),
      div(class = "small text-muted mt-2",
          "Hourly demand comes from an hourly model trained on ERCOT's hourly data (time of day, weekday, ",
          "holidays and hourly weather), scaled so the 24 hours add up to the daily ARIMAX forecast. ",
          "Hourly numbers are demand (GW): how much power the grid must supply in that hour. ",
          "The daily number is energy (GWh): the 24 hourly demands added up.")
    ))
  })

  output$hour_plot <- renderPlot({
    hr <- day_hours()
    ggplot(hr, aes(Time, Power, fill = Power)) +
      geom_col(width = 3000) +
      scale_fill_gradient(low = "#3498db", high = "#e74c3c", guide = "none") +
      scale_x_datetime(date_breaks = "2 hours", labels = hour_label) +
      scale_y_continuous(labels = function(x) fmt(x, 0), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Predicted demand (GW)") +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
  })

  output$hour_strip <- renderUI({
    hr <- day_hours()
    i_peak <- which.max(hr$Power)
    now <- format(Sys.time(), "%Y-%m-%d %H", tz = tz)
    div(class = "hour-strip",
      lapply(seq_len(nrow(hr)), function(j) {
        classes <- c(if (j == i_peak) "peak",
                     if (format(hr$Time[j], "%Y-%m-%d %H") == now) "now")
        hour_tile(hr[j, ], classes)
      })
    )
  })

  output$detail <- renderTable({
    d <- daily()
    data.frame(
      Day                     = paste0(format(d$Date, "%a %b %d"), ifelse(d$Holiday == 1, " (holiday)", "")),
      `Avg temp`              = paste0(round(d$Temp_avg, 1), "°F"),
      `High / Low`            = paste0(round(d$Temp_max), " / ", round(d$Temp_min), "°F"),
      `Dew point`             = paste0(round(d$Dew_avg), "°F"),
      `Wind`                  = paste(round(d$Wind_avg, 1), "mph"),
      `Cloud cover`           = paste0(round(d$Cloud_avg), "%"),
      `Predicted (GWh)`       = fmt(d$Forecast),
      `80% range (GWh)`       = paste(fmt(d$Lo80), "–", fmt(d$Hi80)),
      `Peak hour`             = paste0(hour_label(d$Peak_time), " — ", fmt(d$Peak_GW), " GW"),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
}

shinyApp(ui, server)
