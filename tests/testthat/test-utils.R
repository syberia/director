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

  test_that("it can insert list elements", {
    x <- list(a = 1)
    expect_identical(x %<<% list(b = 2), list(a = 1, b = 2))
    expect_identical(x %<<% list(a = 2), list(a = 2))
  })
})
