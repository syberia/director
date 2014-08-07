context('resource parsers')

test_that("it is able to follow a depth-1 dependency chain", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two')$value() })
    d$register_parser('foo', function() { "test" })
    r <- d$resource('blah/one') # cache the resource info
    expect_equal(r$value(), 'test')
  })
})

test_that("a parser has access to the resource key", {
  within_file_structure(list(blah = list('one.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { resource })
    r <- d$resource('blah/one') 
    expect_equal(r$value(), 'blah/one')
  })
})

test_that("a parser has access to the local sourced variables", {
  within_file_structure(list(blah = list(one.R = 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { list(foo = input$foo, bar = input$bar) })
    r <- d$resource('blah/one') 
    expect_equal(r$value(), list(foo = 1, bar = "a"))
  })
})

test_that("a parser has access to the output value", {
  within_file_structure(list(blah = list(one.R = 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { output })
    r <- d$resource('blah/one') 
    expect_equal(r$value(), 'a')
  })
})

test_that("a parser has access to the resource body", {
  within_file_structure(list(blah = list(one.R = bodystring <- 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { resource_body })
    r <- d$resource('blah/one') 
    expect_equal(r$value(), bodystring)
  })
})

test_that("a parser has access to the director", {
  within_file_structure(list(blah = list(one.R = 'foo <- 1; bar <- "a"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director })
    r <- d$resource('blah/one') 
    expect_identical(r$value(), d)
  })
}) 

test_that("it can ascertain dependencies for a complicated chain", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R'),
                             faz = list('three.R'), floop = list('four.R'),
                             beh.R = "resource('gurp/five')",
                             gurp = list(five.R = 'resource("gurp/six")',
                                         six.R = '"test"')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two')$value() })
    d$register_parser('foo', function() { director$resource('faz/three')$value() })
    d$register_parser('faz', function() { director$resource('floop/four')$value() })
    d$register_parser('floop', function() { director$resource('beh')$value() })
    r <- d$resource('blah/one')
    expect_equal(r$value(), 'test')
    expected <- sort(c("foo/two", "faz/three", "floop/four", "beh", "gurp/five", "gurp/six"))
    expect_identical(sort(r$dependencies()), expected)
  })
})

### These tests go last because they must use Sys.sleep

test_that("it remembers dependencies", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two')$value() })
    d$register_parser('foo', function() { "test" })
    r <- d$resource('blah/one'); r$value()
    r <- d$resource('blah/one'); r$value()
    expect_false(r$modified)
    Sys.sleep(1)
    touch(file.path(tempdir, 'foo', 'two.R'))
    r <- d$resource('blah/one') # cache the resource info
    r$value()
    expect_true(r$modified)
  })
})

test_that("it remembers depth-2 dependencies", {
  within_file_structure(list(blah = list('one.R'), foo = list('two.R'),
                             faz = list('three.R')), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two')$value() })
    d$register_parser('foo', function() { director$resource('faz/three')$value() })
    d$register_parser('faz', function() { "test" })
    r <- d$resource('blah/one'); r$value()
    r <- d$resource('blah/one'); r$value()
    expect_false(r$modified)
    Sys.sleep(1)
    touch(file.path(tempdir, 'faz', 'three.R'))
    r <- d$resource('blah/one'); r$value()
    expect_true(r$modified)
  })
})

test_that("it notices modification of a helper of a dependent resource", {
  within_file_structure(list(blah = list('one.R'), foo = list(two = list('two.R', 'helper.R'))), {
    d <- director(tempdir)
    d$register_parser('blah', function() { director$resource('foo/two')$value() })
    d$register_parser('foo', function() { "test" })
    r <- d$resource('blah/one'); r$value(); 
    r <- d$resource('blah/one'); r$value()
    expect_false(r$modified)
    Sys.sleep(1)
    touch(file.path(tempdir, 'foo', 'two', 'helper.R'))
    r <- d$resource('blah/one'); r$value()
    expect_true(r$modified)
  })
})

