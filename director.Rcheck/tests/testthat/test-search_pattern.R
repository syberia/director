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
  test_that("it correctly applies a regex search pattern", {
    pattern <- search_pattern("[a-z]oo", "regex")
    expect_equal(apply_pattern(pattern, c("foo", "boo", "gaa", "arooga")), c("foo", "boo", "arooga"))
  })

  test_that("it correctly applies an exact search pattern", {
    expect_equal(apply_pattern(search_pattern("foo", "exact"), c("foo", "bar")), "foo")
  })

  test_that("it correctly applies a wildcard search pattern", {
    pattern <- search_pattern("crumb", "wildcard")
    expect_equal(
      apply_pattern(pattern, c("ooo breadcruumbs", "doc Rumbella", "crumm", ".c.r.u.m.b.!")),
      c("ooo breadcruumbs", "doc Rumbella", ".c.r.u.m.b.!")
    )
  })

  test_that("it correctly applies a partial search pattern", {
    pattern <- search_pattern("crumb", "partial")
    expect_equal(
      apply_pattern(pattern, c("ooo breadcrumbs", "docrumbella", "crumm", ".c.r.u.m.b.!")),
      c("ooo breadcrumbs", "docrumbella")
    )
  })

  test_that("it applies idempotence correctly on a complex example", {
    collection <-
      c("a/a", "bc/b", "dummy/foo/bin/bin", "dao/die/die", "dao/die/dee",
        "doo/die/dee/dum", "doo/die/dee/dee", "dao/die/dee/daw")
    output <- apply_pattern(search_pattern("", "idempotence"), collection)
    expect_identical(sort(output),
      sort(c("a", "dummy/foo/bin", "dao/die", "doo/die/dee", "bc/b", "dao/die/dee/daw")))
  })

  test_that("it can determine one idempotent resource", {
    expect_identical(apply_pattern(search_pattern("", "idempotence"), "foo/foo"), "foo")
  })
})

