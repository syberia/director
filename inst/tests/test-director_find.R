context('director$find method')
require(testthatsomemore)

test_that('it correctly finds no files in an empty directory', {
  within_file_structure(list(), { d <- director(tempdir)
    expect_equal(0, length(d$find('')), info = 'No files should have been found.')
  })
})

test_that('it correctly finds a simple file in the root', {
  within_file_structure(list('hello.R'), { d <- director(tempdir)
    expect_identical('/hello', d$find('hello'), info = 'The find method should have found the file "hello".')
  })
})
