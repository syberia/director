context('resource parsers')
library(testthatsomemore)

test_that("it notices modification of a helper of a dependent resource", {
  within_file_structure(list(blah = list('one.R'), foo = list(two = list('two.R', 'helper.R'))), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two') })
    d$register_parser('foo', function() { "test" })
    replicate(2, d$resource('blah/one'))
    expect_false(d$resource("blah/one", modification_tracker.touch = FALSE,
                            dependency_tracker.return = "any_dependencies_modified"))
    Sys.sleep(1)
    touch(file.path(tempdir, 'foo', 'two', 'helper.R'))
    d$resource('blah/one')
    expect_true(d$resource("blah/one", modification_tracker.touch = FALSE,
                           dependency_tracker.return = "any_dependencies_modified"))
  })
})

test_that("it remembers dependencies", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two') })
    d$register_parser('foo', function() { "test" })
    replicate(2, d$resource('blah/one'))
    expect_false(d$resource("blah/one", modification_tracker.touch = FALSE,
                            dependency_tracker.return = "any_dependencies_modified"))
    Sys.sleep(1)
    touch(file.path(tempdir, 'foo', 'two.R'))
    d$resource('blah/one')
    expect_true(d$resource("blah/one", modification_tracker.touch = FALSE,
                           dependency_tracker.return = "any_dependencies_modified"))
  })
})

describe("erroring on invalid inputs", {
  test_that("it errors when a non-character path is passed", {
    d <- director(tempdir())
    expect_error(d$register_parser(NULL), "parameter must be")
    expect_error(d$register_parser(1), "parameter must be")
    expect_error(d$register_parser(FALSE), "parameter must be")
    assert(d$register_parser("foo"))
  })

  test_that("it errors when a non-function parser is passed", {
    d <- director(tempdir())
    expect_error(d$register_parser("", NULL), "parameter must be")
    expect_error(d$register_parser("", 1), "parameter must be")
    expect_error(d$register_parser("", FALSE), "parameter must be")
    assert(d$register_parser("foo", function() { }))
  })

})

test_that("it is able to follow a depth-1 dependency chain", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two') })
    d$register_parser('foo', function() { "test" })
    expect_equal(d$resource("blah/one"), 'test')
  })
})

test_that("a parser has access to the resource key", {
  within_file_structure(list(blah = list('one.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { resource })
    expect_equal(d$resource("blah/one"), 'blah/one')
  })
})

test_that("a parser has access to the local sourced variables", {
  within_file_structure(list(blah = list(one.R = 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { list(foo = input$foo, bar = input$bar) })
    expect_equal(d$resource("blah/one"), list(foo = 1, bar = "a"))
  })
})

test_that("a parser has access to the output value", {
  within_file_structure(list(blah = list(one.R = 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { output })
    expect_equal(d$resource("blah/one"), 'a')
  })
})

test_that("a parser has access to the director", {
  within_file_structure(list(blah = list(one.R = 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director })
    expect_identical(d$resource("blah/one"), d)
  })
}) 

test_that("it can ascertain dependencies for a complicated chain", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R'),
                             faz = list('three.R'), floop = list('four.R'),
                             beh.R = "resource('gurp/five')",
                             gurp = list(five.R = 'resource("gurp/six")',
                                         six.R = '"test"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two') })
    d$register_parser('foo', function() { director$resource('faz/three') })
    d$register_parser('faz', function() { director$resource('floop/four') })
    d$register_parser('floop', function() { director$resource('beh') })
    expect_equal(d$resource("blah/one"), 'test')
    expected <- sort(c("foo/two", "faz/three", "floop/four", "beh", "gurp/five", "gurp/six"))
    # expect_identical(sort(r$dependencies()), expected)
  })
})

### These tests go last because they must use Sys.sleep

test_that("it remembers depth-2 dependencies", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R'),
                             faz = list('three.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two') })
    d$register_parser('foo', function() { director$resource('faz/three') })
    d$register_parser('faz', function() { "test" })
    d$resource('blah/one')
    d$resource('blah/one')
    expect_false(d$resource("blah/one", modification_tracker.touch = FALSE,
                            dependency_tracker.return = "any_dependencies_modified"))
    Sys.sleep(1)
    touch(file.path(tempdir, 'faz', 'three.R'))
    d$resource('blah/one')
    expect_true(d$resource("blah/one", modification_tracker.touch = FALSE,
                           dependency_tracker.return = "any_dependencies_modified"))
  })
})

test_that("supports missing parsers", {
  within_file_structure(list(), { d <- director(tempdir)
    assert(d$register_parser('test'))
  })
})

