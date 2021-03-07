test_that("names_from_formula works", {
  # Correct argument matching
  expect_error(names_from_formula("lon-quod"))
  # Output class
  expect_type(names_from_formula("lin-quad"), "character")
})

test_that("to title works", {
  expect_equal(to_title("hello world"), "Hello World")
})
