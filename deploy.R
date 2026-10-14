# Publish the app to shinyapps.io
#
# One-time setup:
#   1. Create a free account at https://www.shinyapps.io
#   2. In shinyapps.io: your name (top right) -> Tokens -> Show -> Copy to clipboard
#   3. Paste that rsconnect::setAccountInfo(...) line into the RStudio console and run it.
#      (It holds a secret token, so don't save it in this file or share it.)
#
# Then, any time you want to publish or update:   source("deploy.R")

library(rsconnect)

deployApp(
  appDir   = ".",
  appName  = "texas-power-forecast",          # becomes https://<account>.shinyapps.io/texas-power-forecast/
  appTitle = "Texas Power Usage — 7-Day Forecast",
  # Only what the app needs (not the 100+ MB of training data)
  appFiles = c("app.R", "features.R", "power_models.rds"),
  forceUpdate = TRUE
)
