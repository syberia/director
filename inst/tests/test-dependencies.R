context('resource dependencies')

test_that('it correctly identifies a resource run-time dependency for a simple example', {
  within_file_structure(list(bar.R = 'resource("foo")', 'foo.R'), { d <- director(tempdir)
    (r <- d$resource('bar'))$value()
    expect_identical(r$dependencies(), 'foo')
  })
})

test_that('it correctly identifies a parser run-time dependency for a simple example', {
  within_file_structure(list('bar.R', 'foo.R'), { d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    (r <- d$resource('bar'))$value()
    expect_identical(r$dependencies(), 'foo')
  })
})

test_that('it correctly identifies a resource and parser run-time dependency for a simple example', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R'), { d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    (r <- d$resource('bar'))$value()
    expect_identical(sort(r$dependencies()), sort(c('foo', 'baz')))
  })
})

test_that('it correctly identifies a nested run-time dependency', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R', 'bum.R'), {
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    d$register_parser('foo', function() director$resource('baz'))
    d$register_parser('baz', function() director$resource('bum'))
    (r <- d$resource('bar'))$value()
    expect_identical(sort(r$dependencies()), sort(c('foo', 'baz', 'bum')))
    expect_identical(sort(r$dependencies()), sort(c('foo', 'baz', 'bum')))
  })
})

test_that('it correctly identifies a run-time dependency for an intermediate resource', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R', 'bum.R'), {
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo')$value())
    d$register_parser('foo', function() director$resource('baz')$value())
    d$register_parser('baz', function() director$resource('bum')$value())
    
    (r <- d$resource('foo'))$value()
    expect_identical(sort(r$dependencies()), sort(c('baz', 'bum')))
  })
})

test_that('it maintains hierarchical dependencies', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R', 'bum.R'), {
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo')$value())
    d$register_parser('foo', function() director$resource('baz')$value())
    d$register_parser('baz', function() director$resource('bum')$value())
    (r <- d$resource('bar'))$value()
    expect_identical(sort(r$dependencies()), sort(c('foo', 'baz', 'bum')))
    (r <- d$resource('foo'))$value()
    expect_identical(sort(r$dependencies()), sort(c('baz', 'bum')))
    (r <- d$resource('baz'))$value()
    expect_identical(sort(r$dependencies()), sort(c('bum')))
    (r <- d$resource('bum'))$value()
    expect_identical(r$dependencies(), character(0))
  })
})

