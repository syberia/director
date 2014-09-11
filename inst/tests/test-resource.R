context('resource refClass')
library(testthatsomemore)
# TODO: (RK) Test the ... parameter of $value() / $compile() / $parse() 

test_that('calling $value() twice keeps a cache', {
  within_file_structure(list(blah.r = 'list2env(list(x = 1))'), { d <- director(tempdir)
    r <- d$resource('blah')
    v <- r$value()
    v$x <- 2 
    expect_identical(r$value()$x, 2)
  })
})

test_that('calling $value(recompile. = TRUE) results in recompilation', {
  within_file_structure(list(blah.r = 'list2env(list(x = 1))'), { d <- director(tempdir)
    r <- d$resource('blah')
    v <- r$value()
    v$x <- 2
    expect_identical(r$value(recompile. = TRUE)$x, 1)
  })
})

test_that('it can process a virtual resource', {
  within_file_structure(list(), { d <- director(tempdir)
    d$register_preprocessor('blah', function() { 'virtual resource' })
    expect_identical(d$resource('blah!')$value(), 'virtual resource')
  })
})

test_that('it errors on a non-existent resource', {
  within_file_structure(list(), { d <- director(tempdir)
    expect_error(d$resource('blah')$value(), 'Cannot find')
  })
})
