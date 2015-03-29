context('is.idempotent_directory')
require(testthatsomemore)

test_that('it correctly identifies an idempotent directory', {
  within_file_structure(list(one = list('one.R')),
    expect_true(is.idempotent_directory(file.path(tempdir, 'one'))))
})

test_that('it correctly identifies a non-idempotent directory', {
  within_file_structure(list(), expect_false(is.idempotent_directory(tempdir)))
})

