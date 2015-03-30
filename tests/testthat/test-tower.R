context("tower")
library(testthatsomemore)

cool_function <- function(object, ...) yield()
uncool_function <- function(object, ...) breadcrumbs()

describe("handling invalid inputs", {
  test_that("it errors if a non-list is passed", {
    expect_error(tower(1))
    expect_error(tower(NULL))
    expect_error(tower(c(TRUE, FALSE)))
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

describe("tower creation", {
  test_that("it can create the identity tower", {
    assert(tower())
  })

  test_that("it can create a simple tower", {
    assert(tower(list(cool_function)))
  })

  test_that("it can create a 2-tower", {
    assert(tower(list(cool_function, cool_function)))
  })

  test_that("it can create a tower from different functions", {
    assert(tower(list(cool_function, function(object, ..., blah) {
      yield %to% me
    })))
    check that simple towers can be created
})

describe("running tower examples", {  
})

