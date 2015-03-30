context("tower")
library(testthatsomemore)

cool_function <- function(object, ...) yield()
uncool_function <- function(object, ...) breadcrumbs()
double_function <- function(object, ...) {
  object <- 2 * yield()
  object
}

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
  })
})

describe("running tower examples", {  
  test_that("it can run the identity tower", {
    expect_equal(tower()(1), 1)
    expect_equal(tower()(NULL), NULL)
    expect_equal(tower()(list(), 5), list())
  })

  test_that("it can run a 1-tower", {
    expect_equal(tower(list(double_function))(1), 2)
  })

  test_that("it can run a 2-tower", {
    expect_equal(tower(list(double_function, double_function))(1), 4)
  })
})

