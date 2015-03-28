context('director$.filename')
library(testthatsomemore)

test_that('it correctly completes the name of an extensionless nested file (uppercase .R)', {
  within_file_structure(list(blah = list('test.R')), {
    d <- director(tempdir)
    # On some systems, extensions are case-insensitive
    expect_true(d$.filename('blah/test') %in%
                paste0('blah/test.', c('r', 'R')))
  })
})

test_that('it correctly completes the absolute name of an extensionless nested file (uppercase .R)', {
  within_file_structure(list(blah = list('test.R')), {
    d <- director(tempdir)
    # On some systems, extensions are case-insensitive
    expect_true(normalizePath(d$.filename('blah/test', absolute = TRUE)) %in%
                normalizePath(file.path(tempdir, paste0('blah/test.', c('r', 'R')))))
  })
})

# Uncomment this test if we realize we need to allow case-sensitive extensions.
# https://github.com/robertzk/director/issues/17

# test_that('it correctly completes the name of an extensionless nested file (lowercase .r)', {
#  within_file_structure(list(blah = list('test.r')), {
#    d <- director(tempdir)
#    expect_equal(d$.filename('blah/test'), file.path(d$.root, 'blah/test.r'))
#  })
# })
