context("search_pattern")

test_that("it errors if an invalid method is provided", {
  invalid_methods <- list(NULL, 5, "bloo", "partia", a ~ b)
  lapply(invalid_methods, function(method) {
    expect_error(search_pattern("", method), "must be one of ")
  })
})

