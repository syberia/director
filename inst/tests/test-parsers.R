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
    writeLines('""', file.path(tempdir, 'foo', 'two.R'))
    r <- d$resource('blah/one') # cache the resource info
    r$value()
    expect_true(r$modified)
  })
})

