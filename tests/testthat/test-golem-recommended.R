library(golem)

test_that("app ui", {
  ui <- app_ui()
  expect_shinytaglist(ui)
})

test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
})

# Configure this test to fit your need
# Commented, as devtools::check() fails at x$is_alive() (see https://github.com/ThinkR-open/golem/issues/587)
# test_that(
#   "app launches",{
#     skip_on_cran()
#     skip_on_travis()
#     skip_on_appveyor()
#     x <- processx::process$new(
#       "R",
#       c(
#         "-e",
#         "pkgload::load_all(here::here());run_app()"
#       )
#     )
#     Sys.sleep(5)
#     expect_true(x$is_alive())
#     x$kill()
#   }
# )
