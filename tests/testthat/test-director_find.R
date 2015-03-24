context('director$find method')
require(testthatsomemore)

test_that('it correctly finds no files in an empty directory', {
  within_file_structure(list(), { d <- director(tempdir)
    expect_equal(0, length(d$find('')), info = 'No files should have been found.')
  })
})

test_that('it correctly finds a simple file in the root with the exact method', {
  within_file_structure(list('hello.R'), { d <- director(tempdir)
    expect_identical('hello', d$find('hello', method = 'exact'),
                     info = 'The find method should have found the file "hello".')
  })
})

test_that('it correctly finds a simple file and not the other in the root with the partial method', {
  within_file_structure(list('hello.R', 'boo.R'), { d <- director(tempdir)
    expect_identical('hello', d$find('ell', method = 'partial'),
                     info = 'The find method should have found just the file "hello".')
  })
})

test_that('it correctly finds a simple nested file with the partial method', {
  within_file_structure(list(uno = list('dos.R')), { d <- director(tempdir)
    expect_identical('uno/dos', d$find('o/do', method = 'partial'),
                     info = 'The find method should have found the nested resource /uno/dos.')
  })
})

test_that('it correctly finds a nested file using wildcard search', {
  within_file_structure(list(uno = list(dos = list(tres = list('quatro.R')))), { d <- director(tempdir)
    expect_identical('uno/dos/tres/quatro', d$find('ooeuao', method = 'wildcard'),
                     info = 'The find method should have found the nested resource /uno/dos/tres/quatro.')
  })
})

test_that('it correctly does not find a nested file using wildcard search', {
  within_file_structure(list(uno = list(dos = list(tres = list('quatro.R')))), { d <- director(tempdir)
    expect_equal(0, length(d$find('ooeuaoo', method = 'wildcard')),
                 info = 'The find method should not have found the nested resource /uno/dos/tres/quatro.')
  })
})

test_that('it correctly uses a base to look for an exact match', {
  within_file_structure(list(uno = list('dos.R')), { d <- director(tempdir)
    expect_identical('uno/dos', d$find('dos', base = 'uno', method = 'exact'),
      info = 'Since we are looking for dos with base uno, it should be found.')
  })
})

test_that('it correctly uses a base to look for a partial match', {
  within_file_structure(list(uno = list(dos = list('tres.R'))), { d <- director(tempdir)
    expect_identical('uno/dos/tres', d$find('os/t', base = 'uno', method = 'partial'),
      info = 'Since we are looking for "os/t" with base uno, it should find /uno/dos/tres.')
  })
})

test_that('it correctly uses a base to look for a wildcard match', {
  within_file_structure(list(uno = list(dos = list('tres.R'))), { d <- director(tempdir)
    expect_identical('uno/dos/tres', d$find('ots', base = 'uno', method = 'wildcard'),
      info = 'Since we are looking for "os/t" with base uno, it should find /uno/dos/tres.')
  })
})

test_that('correctly finds files ordered by modification time', {
  within_file_structure(list('hello.R', 'boo.R'), { d <- director(tempdir)
    Sys.sleep(1)
    writeLines("#", file.path(tempdir, "hello.R"))
    expect_identical(c('hello', 'boo'), d$find('', by_mtime = TRUE))
  })
})

test_that('ignores modification time when by_mtime = FALSE', {
  within_file_structure(list('hello.R', 'boo.R'), { d <- director(tempdir)
    Sys.sleep(1)
    writeLines("#", file.path(tempdir, "hello.R"))
    expect_identical(c('boo', 'hello'), d$find('', by_mtime = FALSE))
  })
})
