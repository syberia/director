context('any_is_substring_of')

test_that('it correctly identifies a substring', {
  expect_true(any_is_substring_of('test', c('blah', 'te', 'woo')))
})

test_that('it correctly determines lack of a substring equality', {
  expect_false(any_is_substring_of('test', c('blah', 'woo')))
})

