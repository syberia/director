context('cached resources')
library(testthatsomemore)

test_that("pinging a cached resource's dependencies wipes the cache", {
  within_file_structure(list(bar.R = 'cat("bar")', 'foo.R', 'baz.R', 'bum.R'), {
    sink()
    d <- director(tempdir)
    d$register_parser('bar', function() director$resource('foo'), cache = TRUE)
    d$register_parser('foo', function() director$resource('baz'))
    d$register_parser('baz', function() director$resource('bum'))
    expect_output(d$resource('bar'), "^bar(NULL)?$", info = "expected bar")
    expect_output(d$resource('bar'), "^(NULL)?$", info = "expected nothing")
    writeLines("cat('foo')", file.path(tempdir, "bum.R"))
    touch_file(file.path(tempdir, "bum.R"))
    expect_output(d$resource('bar'), "^barfoo", info = "expected barfoo")
  })
})

test_that('it can cache a NULL resource correctly', {
  within_file_structure(list(foo.R = 'cat("only once")'), { d <- director(tempdir)  
    d$register_parser('foo', function() { output }, cache = TRUE)                      
    expect_output(invisible(replicate(5, d$resource('foo'))), '^only once$')
    expect_null(d$resource('foo'))
  })
})

test_that('it can cache a resource correctly', {
  within_file_structure(list(foo.R = 'cat("only once"); list(x = 1)'), { d <- director(tempdir)
    d$register_parser('foo', function() { output }, cache = TRUE)
    expect_output(invisible(replicate(5, d$resource('foo'))), '^only once$')
    expect_identical(d$resource('foo'), list(x = 1))
  })
})
