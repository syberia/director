context('director$resource')
library(testthatsomemore)

test_that('asking for a non-existent resource returns an error', {
  within_file_structure(, { d <- director(tempdir)
    expect_error(d$resource('blah'), 'Cannot find resource')
  })
})

test_that('it can find a simple resource', {
  within_file_structure(list('blah.R'), { d <- director(tempdir)
    assert(d$resource('blah'))
  })
})

test_that('it marks a new resource as modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_true(d$resource('blah', modification_tracker.return = "modified"))
  })
})

test_that('it marks a new resource as modified when referenced with .r', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_true(d$resource('blah', modification_tracker.return = "modified"))
  })
})

# test modified key in resource list

test_that('it marks an old resource as not modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    expect_false(d$resource('blah.r', modification_tracker.return = "modified"))
  })

  within_file_structure(list('blah.r'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    expect_false(d$resource('blah.R', modification_tracker.return = "modified"))
  })
})

test_that('it returns the correct current resource info',  {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_identical(d$resource('blah', modification_tracker.return = "mtime"),
                     file.info(file.path(tempdir, 'blah.r'))$mtime)
  })
})

# TODO: (RK) Replace with modification_tracker tests

# test_that('it modifies the cache if soft = FALSE', {
#   within_file_structure(list('blah.r'), { d <- director(tempdir)
#     d$resource('blah') # cache the resource info
#     writeLines('test <- 1', file.path(tempdir, 'blah.r'))
#     d$resource('blah', soft = FALSE)
#     expect_identical(d$resource('blah')$cached$body, 'test <- 1')
#   })
# })
# 
# test_that('it does not modify the cache if soft = TRUE', {
#   within_file_structure(list('blah.r'), { d <- director(tempdir)
#     d$resource('blah') # cache the resource info
#     writeLines('test <- 1', file.path(tempdir, 'blah.r'))
#     d$resource('blah', soft = TRUE) # (hopefully?) without caching 
#     expect_identical(d$resource('blah')$cached$body, '')
#   })
# })

# TODO: (RK) Re-investigate, but I think this is deprecated.
# test_that('calling $value() results in use of the provided environment', {
#   within_file_structure(list(blah.r = 'test'), { d <- director(tempdir)
#     r <- d$resource('blah', provides = list(test = test <- 1))
#     expect_identical(r$value(), test)
#   })
# })

test_that('resources do not have access to the top environment', {
  name <- '*tmp.for.director.tests*'
  within_file_structure(list(blah.r = paste0('`', name, '`')), { d <- director(tempdir)
    assign(name, 'test', envir = topenv())
    expect_error(d$resource('blah'), "object [^ ]+ not found")
    suppressWarnings(rm(name, envir = topenv()))
  })
})

### These tests go last because they must use Sys.sleep

test_that('it marks a touched resource as modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    r <- d$resource('blah') 
    expect_false(d$resource('blah', modification_tracker.return = "modified"))
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah.r'))
    expect_true(d$resource('blah', modification_tracker.return = "modified"))
  })
})

test_that('modification of resource helpers is reported correctly', {
  within_file_structure(list(blah = list('blah.r', 'helper.r')), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah', 'helper.r'))
    expect_true(d$resource('blah', modification_tracker.touch = FALSE,
                           modification_tracker.return = "modified"))
  })
})

test_that('modified is FALSE if both get modified and a followup second check is made', {
  within_file_structure(list(blah = list('blah.r', 'helper.r', 'helper2.r')), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah', 'helper.r'))
    writeLines('', file.path(tempdir, 'blah', 'helper2.r'))
    d$resource('blah') # trigger cache hit
    d$resource('blah') # trigger cache hit
    expect_false(d$resource('blah', modification_tracker.touch = FALSE,
                            modification_tracker.return = "modified"))
  })
})

test_that('it can skip parsing', {
  within_file_structure(list(blah.R = '"hello"'), { d <- director(tempdir)
    d$register_parser('blah', function() "world")
    r <- d$resource('blah') # cache the resource info
    expect_identical(d$resource('blah', parse. = FALSE), 'hello')
  })
})

# TODO: (RK) Revisit this test after helper inject.
# test_that('a sourced resource can access helpers', {
#   within_file_structure(list(blah = list(blah.R = 'helper("blah/foo")', foo.R = '1')), {
#     d <- director(tempdir)
#     expect_identical(d$resource('blah'), 1)
#   })
# })

test_that('a sourced resource can access existence checks', {
  within_file_structure(list(blah.R = 'resource_exists("blah")',
                             foo.R = 'resource_exists("baz")'), {
    d <- director(tempdir)
    expect_true(d$resource('blah'))
    expect_false(d$resource('foo'))
  })
})

test_that('a sourced resource can pass args', {
  within_file_structure(list(blah.R = ''), {
    d <- director(tempdir)
    d$register_parser('blah', function(args) args$test)
    expect_identical(d$resource('blah', test = 1), 1)
  })
})

