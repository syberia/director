context('resource dependencies')
library(testthatsomemore)

test_that('it correctly identifies a resource run-time dependency for a simple example', {
  within_file_structure(list(bar.R = 'resource("foo")', 'foo.R'), { d <- director(tempdir)
    d$resource('bar')
    expect_identical(d$resource("bar", dependency_tracker.return = "dependencies"), "foo")
  })
})

test_that('it correctly identifies a parser run-time dependency for a simple example', {
  within_file_structure(list('bar.R', 'foo.R'), { d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    d$resource('bar')
    expect_identical(d$resource("bar", dependency_tracker.return = "dependencies"), "foo")
  })
})

test_that('it correctly identifies a resource and parser run-time dependency for a simple example', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R'), { d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    d$resource('bar')
    expect_identical(sort(d$resource("bar", dependency_tracker.return = "dependencies")),
                     c("baz", "foo"))
  })
})

test_that('it correctly identifies a nested run-time dependency', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R', 'bum.R'), {
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    d$register_parser('foo', function() director$resource('baz'))
    d$register_parser('baz', function() director$resource('bum'))
    d$resource('bar')
    expect_identical(sort(d$resource("bar", dependency_tracker.return = "dependencies")),
                     c("baz", "bum", "foo"))
  })
})

test_that('it correctly identifies a run-time dependency for an intermediate resource', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R', 'bum.R'), {
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    d$register_parser('foo', function() director$resource('baz'))
    d$register_parser('baz', function() director$resource('bum'))
    
    d$resource('foo')
    expect_identical(sort(d$resource("foo", dependency_tracker.return = "dependencies")),
                     c("baz", "bum"))
  })
})

test_that('it maintains hierarchical dependencies', {
  within_file_structure(list(bar.R = 'resource("baz")', 'foo.R', 'baz.R', 'bum.R'), {
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'))
    d$register_parser('foo', function() director$resource('baz'))
    d$register_parser('baz', function() director$resource('bum'))
    d$resource('bar')
    expect_identical(sort(d$resource("bar", dependency_tracker.return = "dependencies")),
                     c("baz", "bum", "foo"))
    d$resource('foo')
    expect_identical(sort(d$resource("foo", dependency_tracker.return = "dependencies")),
                     c("baz", "bum"))
    d$resource('baz')
    expect_identical(sort(d$resource("baz", dependency_tracker.return = "dependencies")), "bum")
    d$resource('bum')
    expect_identical(sort(d$resource("bum", dependency_tracker.return = "dependencies")), character(0))
  })
})

