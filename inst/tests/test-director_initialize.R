context('director_initialize')
require(testthatsomemore)

test_that('it errors when a directory does not exist', {
  expect_error(director('-non-existent-'), 'directory does not exist')
})

test_that('it errors when given a filename rather than a directory', {
  within_file_structure(list('somefile'),
    expect_error(director(file.path(tempdir, 'somefile')), 'that is a file')                        
  )
})

