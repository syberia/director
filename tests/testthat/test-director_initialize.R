context("director$initialize")
require(testthatsomemore)

test_that("it errors when a directory does not exist", {
  expect_error(director("-non-existent-"), "directory does not exist")
})

test_that("it errors when given a filename rather than a directory", {
  within_file_structure(list("somefile"),
    expect_error(director(file.path(tempdir, "somefile")), "that is a file")
  )
})

test_that("it is not possible to call any other method on the director class than $new", {
  expect_error(director$foo())
  expect_error(director$classname)
  expect_error(director$active)
  expect_error(director$inherit)
  expect_error(director$private)
  expect_error(director$public)
  expect_error(director$parent_env)
  expect_error(director$lock)
})

test_that("is.director works", {
  expect_false(is.director(list()))
  expect_true(is.director(director$new(tempdir())))
})
