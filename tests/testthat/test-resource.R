context("director_resource")
library(testthatsomemore)
# TODO: (RK) Test the ... parameter of $value() / $compile() / $parse() 

test_that("calling $value(recompile. = TRUE) results in recompilation", {
  within_file_structure(list(blah.r = "list2env(list(x = 1))"), { d <- director(tempdir)
    d$register_parser("", function(output) { output }, cache = TRUE)
    v <- d$resource("blah")
    v$x <- 2
    expect_equal(d$resource("blah")$x, 2)
    expect_equal(d$resource("blah", recompile. = TRUE)$x, 1)
  })
})

test_that("it can process a virtual resource", {
  within_file_structure(list(), { d <- director(tempdir)
    d$register_preprocessor("blah", function() { "virtual resource" })
    expect_identical(d$resource("blah!"), "virtual resource")
  })
})

test_that("it errors on a non-existent resource", {
  within_file_structure(list(), { d <- director(tempdir)
    expect_error(d$resource("blah"), "Cannot find")
  })
})

describe("process_resource", {
  test_that("it errors if a non-resource is passed", {
    for (x in list("foo", list(), 5, FALSE)) 
      expect_error(process_resource(x), "no applicable method")
  })
})
