# Build Electron executable containing the Shiny App

build_path <- tempdir()

my_package <- system.file(package = "biodosetools")

electricShine::electrify(
  app_name = "Biodose Tools",
  short_description = "Shiny App To Be Used By Biological Dosimetry Laboratories",
  semantic_version = "3.3.1",
  build_path = build_path,
  cran_like_url = "https://cran.r-project.org",
  function_name = "run_app",
  local_package_path = my_package,
  package_install_opts = list(type = "binary")
)
