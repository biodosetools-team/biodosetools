# Checks ----

# Check whole package
devtools::check()

# Run tests
devtools::test()
covr::package_coverage(quiet = FALSE)


# Tests ----

# Add one line by test you want to create
usethis::use_test("utils")
