context('registry')
require(testthatsomemore)

test_that('it allows NULL as a root', {
  assert(registry(NULL)) 
})

test_that('it correctly errors if a non-character root is passed', {
  expect_error(registry(1337), 'must be initialized with a character')
})

test_that('it correctly errors if a specified root is actually a file', {
  within_file_structure(list('root'),
    expect_error(registry(file.path(tempdir, 'root')), 'must be a dir')
  )
})

