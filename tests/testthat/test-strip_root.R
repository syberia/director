context('strip_root')

test_that('it correctly strips the root path', {
  expect_equal(strip_root('foo/bar', 'foo/bar/test'), 'test')
})

test_that('it correctly strips the root path from a prefix', {
  expect_equal(strip_root('foo/bar', 'foo/bartest'), 'test')
})

