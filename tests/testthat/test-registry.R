context('registry')
require(testthatsomemore)

# Test registry$initialize method

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

within_file_structure(list(), {
  root <- tempdir

  # Test registry$.sanitize_key method

  test_that('the .sanitize_key method errors if the key parameter is non-character', {
    expect_error(registry(root)$.sanitize_key(NULL))
  })

  test_that('the .sanitize_key method returns character(0) on character(0)', {
    expect_identical(registry(root)$.sanitize_key(character(0)), character(0))
  })

  test_that('the .sanitize_key method prevents usage of ".." in keys', {
    expect_error(registry(root)$.sanitize_key('..'), 'cannot contain two consecutive')
  })

  test_that('the .sanitize_key method errors if read = TRUE, soft = FALSE and the key doesnt exists', {
    expect_error(registry(root)$.sanitize_key('foo', read = TRUE, soft = FALSE),
                 'There is no registry item')
  })

  test_that('the .sanitize_key method errors if read = TRUE and the key is a directory', {
    within_file_structure(dir = root, list(some_dir = list()), {
      expect_error(registry(root)$.sanitize_key('some_dir', read = TRUE),
                   'this key points to a directory')
    })
  })

  test_that('the .sanitize_key method errors if read = FALSE and an intermediate parent is a file', {
    within_file_structure(dir = root, list(some_dir = list('some_file')), {
      expect_error(registry(root)$.sanitize_key('some_dir/some_file/nonsense', read = FALSE),
                   'Cannot create registry key')
    })
  })

  test_that('it correctly sanitizes a key for an example file with no parent directory', {
    within_file_structure(dir = root, list('file'), {
      saveRDS('test', path <- file.path(normalizePath(tempdir), 'file'))
      expect_identical(registry(tempdir)$.sanitize_key('file'), path)
    })
  })

  test_that('it correctly sanitizes a key for an example file with a parent directory', {
    within_file_structure(dir = root, list(dir = list('file')), {
      saveRDS('test', path <- file.path(normalizePath(tempdir), 'dir', 'file'))
      expect_identical(registry(tempdir)$.sanitize_key('dir/file'), path)
    })
  })

  test_that('the .sanitize_key method vectorizes', {
    within_file_structure(dir = root, expr = {
      sapply(1:2, function(x)
        saveRDS(paste0('test', x), file.path(tempdir, paste0('file', x))))
      expect_identical(unname(registry(tempdir)$.sanitize_key(c('file1', 'file2'))),
                       file.path(normalizePath(tempdir), c('file1', 'file2')))
    })
  })

  # Test registry$set and get methods

  test_that('a simple key can be retrieved', {
    r <- registry(root)
    r$set('test', value <- list('test', 5))
    expect_identical(r$get('test'), value)
  })

  test_that('a directoried key can be retrieved', {
    r <- registry(root)
    r$set('another/test', value <- list('test', 5))
    expect_identical(r$get('another/test'), value)
  })

  test_that('a directoried key can be retrieved using multiple argument syntax', {
    r <- registry(root)
    r$set('another/test', value <- list('test', 5))
    expect_identical(r$get('another', 'test'), value)
  })

  test_that('retrieval of non-existent key yields NULL when soft = TRUE', {
    r <- registry(root)
    expect_null(r$get('non-existent key', soft = TRUE))
  })

})


