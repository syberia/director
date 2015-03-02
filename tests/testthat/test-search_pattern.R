context("search_pattern")
library(testthatsomemore)

test_that("it errors if an invalid method is provided", {
  invalid_methods <- list(NULL, 5, "bloo", "partia", a ~ b)
  lapply(invalid_methods, function(method) {
    expect_error(search_pattern("", method), "must be one of ")
  })
})

test_that("it does not error on valid methods", {
  valid_methods <- c("wildcard", "partial", "exact", "WILDcard", "parTIAL", "eXact")
  lapply(valid_methods, function(method) {
    assert(search_pattern("", method))
  })
})

test_that("it errors if a non-character pattern is provided", {
  invalid_patterns <- list(NULL, 5, a ~ b, list())
  lapply(invalid_patterns, function(pattern) {
    expect_error(search_pattern(pattern, "wildcard"), "pattern must be")
  })
})
