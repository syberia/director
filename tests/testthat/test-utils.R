context("utils")

describe("enforce_type", {
  test_that("it errors on a simple example as expected", {
    expect_error(enforce_type(x, "logical", "myfunction"), "parameter must be a")
  })
})
