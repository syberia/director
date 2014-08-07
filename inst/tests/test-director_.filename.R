context('director$.filename')

test_that('it correctly completes the name of an extensionless nested file (uppercase .R)', {
  within_file_structure(list(blah = list('test.R')), {
    d <- director(tempdir)
    # On some systems, extensions are case-insensitive
    expect_true(d$.filename('blah/test') %in%
                file.path(d$.root, paste0('blah/test.', c('r', 'R'))))
  })
})

test_that('it correctly completes the name of an extensionless nested file (lowercase .r)', {
  within_file_structure(list(blah = list('test.r')), {
    d <- director(tempdir)
    expect_equal(d$.filename('blah/test'), file.path(d$.root, 'blah/test.r'))
  })
})
