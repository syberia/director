context('cached resources')
library(testthatsomemore)

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

