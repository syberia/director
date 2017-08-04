## Functions in this directory are used to allow parser and preprocessor
## application to resources that do not have corresponding files. By stubbing
## the inputs typically computed from the file, we can use director
## functionality for adhoc purposes without a corresponding presence on the
## file system.

## In a resource mock, we hold a list of stubbed inputs rather than the
## filename.
director_resource_mock <- function(director, mock, defining_environment) {
  if (!is.element("name", names(mock))) {
    stop("When passing a list to the resource method, provide a filename ",
         "to determine which controller and preprocessor to apply.", call. = FALSE)
  }

  structure(list(
    director = director,
    mock = mock,
    name = mock$name,
    defining_environment = defining_environment
  ), class = "director_resource_mock")
}

## Otherwise, we assume it is a mock of a file for ad hoc use of
## a parser or preprocessor.
process_resource.director_resource_mock <- function(resource, ...) {
  tower(
    mock_injects %>>%
    preprocessor %>>%
    parser               
  )(active_resource(resource), ...)
}

mock_injects <- function(object, ...) {
  object$injects %<<% list(
    virtual  = TRUE, # Every mocked resource has no file and is thus virtual.
    modified = TRUE, # The resource changes on each invoked function call.
    any_dependencies_modified = TRUE
  )
  yield()
}

source_function_for_resource.director_resource_mock <- function(resource) {
  function() { eval.parent(quote({ output })) }
}

output_for_resource.director_resource_mock <- function(resource) { 
  resource$output
}

