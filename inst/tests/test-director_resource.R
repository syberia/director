context('director$resource')
library(testthatsomemore)

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

test_that('a resource output has the keys current, cached, value, and modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_true(all(is.element(names(d$resource('blah')),
                    c('current', 'cached', 'value', 'modified'))))
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

test_that('it returns the correct current resource info',  {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_identical(d$resource('blah')$current$info$mtime,
                     file.info(file.path(tempdir, 'blah.r'))$mtime)
  })
})

test_that('it returns the correct cached resource info',  {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    expect_null(d$resource('blah')$cached$info)
    expect_identical(d$resource('blah')$cached$info$mtime, 
      file.info(file.path(tempdir, 'blah.r'))$mtime)
  })
})

test_that('it returns the correct current resource body',  {
  within_file_structure(list(blah.r = bod <- 'test <- 1'), { d <- director(tempdir)
    expect_identical(d$resource('blah')$current$body, bod)
  })
})

test_that('it returns the correct cached resource body',  {
  within_file_structure(list(blah.r = bod <- 'test <- 1'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    writeLines('test <- 2', file.path(tempdir, 'blah.r'))
    expect_identical(d$resource('blah')$cached$body, bod)
  })
})

test_that('it returns the correct modified resource body',  {
  within_file_structure(list(blah.r = 'test <- 1'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    writeLines(bod2 <- 'test <- 2', file.path(tempdir, 'blah.r'))
    expect_identical(d$resource('blah')$current$body, bod2)
  })
})

test_that('it returns no body if body = FALSE', {
  within_file_structure(list(blah.r = 'test <- 1'), { d <- director(tempdir)
    expect_null(d$resource('blah', body = FALSE)$current$body)
  })
})


test_that('it modifies the cache if soft = FALSE', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    writeLines('test <- 1', file.path(tempdir, 'blah.r'))
    d$resource('blah', soft = FALSE)
    expect_identical(d$resource('blah')$cached$body, 'test <- 1')
  })
})

test_that('it does not modify the cache if soft = TRUE', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    writeLines('test <- 1', file.path(tempdir, 'blah.r'))
    d$resource('blah', soft = TRUE) # (hopefully?) without caching 
    expect_identical(d$resource('blah')$cached$body, '')
  })
})

test_that('different uses of the same filename keep the same cache', {
  within_file_structure(list('blah.r', 'blah2.r'), { d <- director(tempdir)
    d$resource('blah') # cache the resource info
    expect_false(is.null(d$resource('/blah')$cached))
    d$resource('blah2.r') # cache the resource info
    expect_false(is.null(d$resource('blah2')$cached))
  })
})

test_that('calling $value() results in use of the provided environment', {
  within_file_structure(list(blah.r = 'test'), { d <- director(tempdir)
    r <- d$resource('blah', provides = list(test = test <- 1))
    expect_identical(r$value(), test)
  })
})

test_that('resources do not have access to the top environment', {
  name <- '*tmp.for.director.tests*'
  within_file_structure(list(blah.r = paste0('`', name, '`')), { d <- director(tempdir)
    assign(name, 'test', envir = topenv())
    r <- d$resource('blah')
    expect_error(r$value(), "object [^ ]+ not found")
    suppressWarnings(rm(name, envir = topenv()))
  })
})

### These tests go last because they must use Sys.sleep

test_that('it marks a touched resource as modified', {
  within_file_structure(list('blah.r'), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah.r'))
    expect_true(d$resource('blah')$modified)
  })
})

test_that('modification of resource helpers is reported correctly', {
  within_file_structure(list(blah = list('blah.r', 'helper.r')), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah', 'helper.r'))
    expect_true(d$resource('blah')$modified)
  })
})

test_that('modified is FALSE if both get modified and a followup second check is made', {
  within_file_structure(list(blah = list('blah.r', 'helper.r', 'helper2.r')), { d <- director(tempdir)
    r <- d$resource('blah') # cache the resource info
    Sys.sleep(1) # Annoying, but no other way because mtime precision is seconds
    writeLines('', file.path(tempdir, 'blah', 'helper.r'))
    writeLines('', file.path(tempdir, 'blah', 'helper2.r'))
    d$resource('blah') # trigger cache hit
    expect_false(d$resource('blah')$modified)
  })
})

test_that('it can skip parsing', {
  within_file_structure(list(blah.R = '"hello"'), { d <- director(tempdir)
    d$register_parser('blah', function() "world")
    r <- d$resource('blah') # cache the resource info
    expect_identical(d$resource('blah')$value(parse. = FALSE), 'hello')
  })
})

test_that('a sourced resource can access helpers', {
  within_file_structure(list(blah = list(blah.R = 'helper("blah/foo")', foo.R = '1')), {
    d <- director(tempdir)
    expect_identical(d$resource('blah')$value(), 1)
  })
})

