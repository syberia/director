context('preprocessors')
library(testthatsomemore) 

test_that("it is able to register a preprocessor", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function() { 'test' })
    r <- d$resource('blah/one')
    expect_equal(r$value(), 'test')
  })
})

test_that("it can use the trivial preprocessor", {
  within_file_structure(list(blah = list(one.R = '"test"')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function() { do.call(base::source, source_args)$value })
    r <- d$resource('blah/one')
    expect_equal(r$value(), 'test')
  })
})

test_that("it can use a preprocessor to do injection", {
  within_file_structure(list(blah = list(one.R = 'test')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function() {
      source_args$local$test <- 'test'
      do.call(base::source, source_args)$value
    })
    r <- d$resource('blah/one')
    expect_equal(r$value(), 'test')
  })
})

test_that("it can use a preprocessor to pass information to a parser", {
  within_file_structure(list(blah = list(one.R = '"test"')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function() {
      preprocessor_output$body <- readLines(source_args[[1]])
      do.call(base::source, source_args)$value
    })
    d$register_parser('blah', function() { preprocessor_output$body })
    r <- d$resource('blah/one')
    expect_equal(r$value(), '"test"')
  })
})

test_that("the 'source' shortcut works", {
  within_file_structure(list(blah = list(one.R = '"test"')), {
    d <- director(tempdir)
    d$register_preprocessor('blah', function() { source() })
    r <- d$resource('blah/one')
    expect_equal(r$value(), 'test')
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

