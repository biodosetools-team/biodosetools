# Deploy app.R to ShinyApps

rsconnect::deployApp(
  appName = "biodosetools-v3",
  appDir = here::here(),
  appPrimaryDoc = "dev/shiny-redirect/app.R",
  appFileManifest = "dev/shiny-redirect/app_file_manifest.txt"
)
