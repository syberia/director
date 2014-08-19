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

test_that('it correctly finds a simple file and not the other in the root', {
  within_file_structure(list('hello.R', 'boo.R'), { d <- director(tempdir)
    expect_identical('/hello', d$find('ell'), info = 'The find method should have found just the file "hello".')
  })
})

test_that('it correctly finds a simple nested file', {
  within_file_structure(list(uno = list('dos.R')), { d <- director(tempdir)
    expect_identical('/uno/dos', d$find('o/do'),
                     info = 'The find method should have found the nested resource /uno/dos.')
  })
})

test_that('it correctly finds a nested file using wildcard search', {
  within_file_structure(list(uno = list(dos = list(tres = list('quatro.R')))), { d <- director(tempdir)
    expect_identical('/uno/dos/tres/quatro', d$find('ooeuao'),
                     info = 'The find method should have found the nested resource /uno/dos/tres/quatro.')
  })
})
