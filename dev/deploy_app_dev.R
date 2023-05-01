# Deploy app.R to ShinyApps

rsconnect::deployApp(
  appName = "biodosetools-dev",
  appDir = here::here(),
  appPrimaryDoc = "dev/shiny-dev/app.R",
  appFileManifest = "dev/shiny-dev/app_file_manifest.txt"
)
