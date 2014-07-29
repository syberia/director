context('director$exists method')
require(testthatsomemore)

test_that('it correctly determines trivial non-existence', {
  within_file_structure(list(), expect_false(director(tempdir)$exists('foo')))
})

test_that('it correctly determines existence of a non-idempotent resource', {
  within_file_structure(list('foo.R'), {
    expect_true(director(tempdir)$exists('foo'))
    expect_true(director(tempdir)$exists('foo.r'))
    expect_true(director(tempdir)$exists('foo.R'))
  })
})

test_that('it correctly determines existence of an idempotent resource', {
  within_file_structure(list(foo = list('foo.R')), {
    expect_true(director(tempdir)$exists('foo'))
    expect_true(director(tempdir)$exists('foo.r'))
    expect_true(director(tempdir)$exists('foo.R'))
  })
})

test_that('it correctly determines non-existence of a helper', {
  within_file_structure(list(foo = list('foo.R', 'helper')), {
    expect_false(director(tempdir)$exists('foo/helper'))
    expect_false(director(tempdir)$exists('foo/helper.r'))
    expect_false(director(tempdir)$exists('foo/helper.R'))
  })
})

test_that('it warns when an idempotent and non-idempotent version exist', {
  within_file_structure(list(foo = list('foo.R', 'helper'), 'foo.R'), {
    expect_true(suppressWarnings(director(tempdir)$exists('foo')))
    expect_warning(director(tempdir)$exists('foo'), "There is both")
  })
})

