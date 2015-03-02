context("search_pattern")
library(testthatsomemore)

describe("search_pattern S3 class", {
  test_that("it errors if an invalid method is provided", {
    invalid_methods <- list(NULL, 5, "bloo", "partia", a ~ b)
    lapply(invalid_methods, function(method) {
      expect_error(search_pattern("", method), "must be|Invalid search")
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

  test_that("it produces an object of type search_pattern", {
    expect_is(search_pattern("blub", "exact"), "search_pattern")
  })

  test_that("it combines multiple patterns into one", {
    expect_is(search_pattern(c("fish", "goes", "blub"), "exact"), "search_pattern")
  })

  test_that("it combines multiple methods into one", {
    expect_is(search_pattern(c("fish", "goes", "blub"), "exact"), "search_pattern")
  })

  test_that("it combines multiple patterns and methods into the uber pattern", {
    expect_is(search_pattern(c("fish", "goes", "blub"), c("exact", "wildcard")), "search_pattern")
  })
})

describe("applying patterns", {
  # TODO: (RK) Fill in these tests.

#  apply_pattern.idempotence("",c("a/a", "bc/b", "dummy/foo/bin/bin", "dao/die/die", "dao/die/dee", "do
# o/die/dee/dum", "doo/die/dee/dee", "dao/die/dee/daw"))
# [1] "a"               "dummy/foo/bin"   "dao/die"         "doo/die/dee"     "bc/b"
# [6] "dao/die/dee/daw"
})

