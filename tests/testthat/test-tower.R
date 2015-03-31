context("tower")
library(testthatsomemore)

cool_function <- function(object, ...) yield()
uncool_function <- function(object, ...) breadcrumbs()
double_function <- function(object, ...) {
  object <- 2 * yield()
  object
}
increment_function <- function(object, ...) { yield() + 1 }

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

  test_that("it can run a tower composed of multiple pieces", {
    functions <- list(
      function(object, ...) {
        object <- object + 1
        object <- yield()
        object + 1
      },

      function(object, ...) {
        object <- object * 2
        yield()
      }
    )
    expect_equal(tower(functions)(1), 5)
  })

  test_that("it can run a tower composed of multiple pieces in reverse", {
    functions <- list(
      function(object, ...) {
        object <- object + 1
        object <- yield()
        object + 1
      },

      function(object, ...) {
        object <- object * 2
        yield()
      }
    )
    expect_equal(tower(rev(functions))(1), 4)
  })
})

describe("tower composition notation", {
  test_that("we can compose a 1-tower", {
    expect_equal(double_function %>>% 1, 2)
  })

  test_that("we can compose a 2-tower", {
    expect_equal(double_function %>>% double_function %>>% 1, 4)
  })

  test_that("we can compose a 2-tower composed of different functions", {
    expect_equal(double_function %>>% increment_function %>>% 1, 4)
    expect_equal(increment_function %>>% double_function %>>% 1, 3)
  })
})

describe("destruction actions", {
  test_that("we can catch destruction actions correctly", {
    fn1 <- function(object, ...) {
      on.exit(cat(attr(object, "foo")))
      object <- yield()
      cat(attr(object, "foo") + 1)
    }
    fn2 <- function(object, ...) {
      structure(yield(), foo = 1)
    }
    expect_output(fn1 %>>% fn2 %>>% 1, "21")
  })
})

describe("floating towers", {
  ## Floating towers are of the form fn1 %>>% fn2, which have not been
  ## evaluated on an argument yet.
  test_that("it can create a floating tower of identities", {
    t <- tower(identity2 %>>% identity2)
    expect_is(t, "tower")
    expect_equal(t(1), 1)
  })

  test_that("it can create a floating tower of distinct functions", {
    t <- tower(double_function %>>% increment_function)
    expect_is(t, "tower")
    expect_equal(t(1), 4)
  })
})

describe("pre_towers", {
  test_that("determining pre_towers works", {
    expect_true(is.pre_tower(identity2 %>>% identity2))
  })
})
