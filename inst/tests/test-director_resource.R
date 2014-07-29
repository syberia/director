context('director$resource')

test_that('asking for a non-existent resource returns an error', {
  within_file_structure(, { d <- director(tempdir)
    expect_error(d$resource('blah'), 'Cannot find resource')
  })
})

test_that('it can find a simple resource', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    assert(d$resource('blah'))
  })
})

# test modified key in resource list

test_that('it marks a new resource as modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_true(d$resource('blah')$modified)
  })
})

test_that('it marks an old resource as not modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    expect_false(d$resource('blah')$modified)
  })
})

test_that('it marks a touched resource as modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah.r'))
    expect_true(d$resource('blah')$modified)
  })
})



