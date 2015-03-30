context("utils")

describe("enforce_type", {
  test_that("it errors on a simple example as expected", {
    x <- 1
    expect_error(enforce_type(x, "logical", "myfunction"), "parameter must be a")
  })
})

describe("%<<% operator", {
  test_that("it errors unless objects are named", {
    expect_error(list(1) %<<% list(2))
    expect_error(list(a = 1) %<<% list(2))
    expect_error(list(1) %<<% list(a = 2))
  })

  describe("inserting into lists", {
    test_that("it can insert list elements", {
      x <- list(a = 1)
      expect_identical(x %<<% list(b = 2), list(a = 1, b = 2))
      expect_identical(x %<<% list(a = 2), list(a = 2))
    })

    test_that("it can insert environments into lists", {
      x <- list(a = 1)
      expect_identical(x %<<% list2env(list(b = 2)), list(a = 1, b = 2))
      expect_identical(x %<<% list2env(list(a = 2)), list(a = 2))
    })
  })

  describe("inserting into environments", {
    tolist <- function(x) {
      x <- as.list(x)
      x[sort(names(x))]
    }

    test_that("it can insert lists into environments", {
      x <- list2env(list(a = 1))
      expect_identical(tolist(x %<<% list(b = 2)), list(a = 1, b = 2))
      x <- list2env(list(a = 1))
      expect_identical(tolist(x %<<% list(a = 2)), list(a = 2))
    })

    test_that("it can insert environments into environments", {
      x <- list2env(list(a = 1))
      expect_identical(tolist(x %<<% list2env(list(b = 2))), list(a = 1, b = 2))
      x <- list2env(list(a = 1))
      expect_identical(tolist(x %<<% list2env(list(a = 2))), list(a = 2))
    })
  })
})
