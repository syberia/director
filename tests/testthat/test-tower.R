context("tower")

cool_function <- function(object, ...) yield()
uncool_function <- function(object, ...) breadcrumbs()

describe("handling invalid inputs", {
  test_that("it errors if a non-list is passed", {
    expect_error(tower(1))
    expect_error(tower(NULL))
    expect_error(tower(c(TRUE, FALSE)))
  })

  test_that("it errors if a length 0 list is passed", {
    expect_error(tower(list()))
  })

  test_that("it errors if a list with a non-function is passed", {
    expect_error(tower(list(identity, force, 1)))
  })

  test_that("it errors if a function without correct formals is passed", {
    expect_error(tower(list(identity)))
    expect_error(tower(list(cool_function, identity)))
  })

  test_that("it errors if a function without a yield keyword is passed", {
    expect_error(tower(list(cool_function, uncool_function)))
  })
})

