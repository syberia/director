context('director$filename')
library(testthatsomemore)

describe("invalid inputs", {
  test_that("it cannot convert a non-existent resource to a filename", {
    expect_error(director(tempdir())$filename("foo"), "no such resource")
  })
})

test_that('it correctly completes the name of an extensionless nested file (uppercase .R)', {
  within_file_structure(list(blah = list('test.R')), {
    d <- director(tempdir)
    # On some systems, extensions are case-insensitive
    expect_true(d$filename('blah/test') %in%
                paste0('blah/test.', c('r', 'R')))
  })
})

test_that('it correctly completes the absolute name of an extensionless nested file (uppercase .R)', {
  within_file_structure(list(blah = list('test.R')), {
    d <- director(tempdir)
    # On some systems, extensions are case-insensitive
    expect_true(normalizePath(d$filename('blah/test', absolute = TRUE)) %in%
                normalizePath(file.path(tempdir, paste0('blah/test.', c('r', 'R')))))
  })
})

test_that("it correctly converts an idempotent resource with helpers to a filename", {
  within_file_structure(list(foo = list(two = list('two.R', 'helper.R'))), {
    d <- director(tempdir)
    expect_equal(d$filename("foo/two"), "foo/two/two.R")
  })
})

test_that("it can give the enclosing directory of an idempotent resource", {
  within_file_structure(list(foo = list("foo.R")), { d <- director(tempdir)
    expect_equal(d$filename("foo", enclosing = TRUE), "foo")
  })
})

# Uncomment this test if we realize we need to allow case-sensitive extensions.
# https://github.com/robertzk/director/issues/17

# test_that('it correctly completes the name of an extensionless nested file (lowercase .r)', {
#  within_file_structure(list(blah = list('test.r')), {
#    d <- director(tempdir)
#    expect_equal(d$filename('blah/test'), file.path(d$.root, 'blah/test.r'))
#  })
# })
