context('preprocessors')
library(testthatsomemore) 

describe("invalid inputs", {
  test_that("it errors if we try to register a non-scalar path", {
    d <- director(tempdir())
    expect_error(
      d$register_preprocessor(c("foo", "bar"), identity),
      "that is a scalar"
    )
  })

  test_that("it does not allow path overwriting", {
    d <- director(tempdir())
    d$register_preprocessor(c("foo"), identity)
    expect_error(
      d$register_preprocessor(c("foo"), identity),
      "Preprocessor already registered"
    )
  })
})

test_that("it is able to register a preprocessor", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function() { 'test' })
    expect_equal(d$resource('blah/one'), 'test')
  })
})

test_that("it can use the trivial preprocessor", {
  within_file_structure(list(blah = list(one.R = '"test"')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function(filename) { base::source(filename)$value })
    expect_equal(d$resource('blah/one'), 'test')
  })
})

test_that("it can use a preprocessor to do injection", {
  within_file_structure(list(blah = list(one.R = 'test')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function(source_env, filename) {
      source_env$test <- "test"
      base::source(filename, source_env)$value
    })
    expect_equal(d$resource('blah/one'), 'test')
  })
})

test_that("it can use a preprocessor to pass information to a parser", {
  within_file_structure(list(blah = list(one.R = '"test"')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function(preprocessor_output, filename) {
      preprocessor_output$body <- readLines(filename)
      base::source(filename)$value
    })
    d$register_parser('blah', function() { preprocessor_output$body })
    expect_equal(d$resource('blah/one'), '"test"')
  })
})

test_that("the 'source' shortcut works", {
  within_file_structure(list(blah = list(one.R = '"test"')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function(source) { source() })
    expect_equal(d$resource('blah/one'), 'test')
  })
})

test_that("it can tell if a preprocessor exists for a resource", {
  within_file_structure(list(), { d <- director(tempdir)
    d$register_preprocessor('test', function() {} )
    expect_true(d$has_preprocessor('test'))
  })
})

test_that("it can tell if a preprocessor doesn't exists for a resource", {
  within_file_structure(list(), { d <- director(tempdir)
    expect_false(d$has_preprocessor('test'))
  })
})

describe("the `helper` inject", {
  test_that("it works as expected with a pure helper", {
    within_file_structure(list(blah = list(blah.R = "helper('blah/foo')", foo.R = "1")), { d <- director(tempdir)
      expect_equal(d$resource("blah"), 1)
    })
  })

  test_that("it runs the preprocessor but not the parser when sourcing a helper", {
    within_file_structure(list(blah.R = "helper('foo')", foo.R = "x"), { d <- director(tempdir)
      d$register_preprocessor("foo", function(source, source_env) { source_env$x <- 1; source() })
      d$register_parser("foo", function() { stop("should not be called") })
      expect_equal(d$resource("blah"), 1)
    })
  })
})

