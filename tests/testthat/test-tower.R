context("tower")

describe("handling invalid inputs", {
  test_that("it errors if a non-list is passed", {
    expect_error(tower(1))
    expect_error(tower(NULL))
    expect_error(tower(c(TRUE, FALSE)))
  })
})

