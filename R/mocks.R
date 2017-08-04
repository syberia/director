## Functions in this directory are used to allow parser and preprocessor
## application to resources that do not have corresponding files. By stubbing
## the inputs typically computed from the file, we can use director
## functionality for adhoc purposes without a corresponding presence on the
## file system.

## In a resource mock, we hold a list of stubbed inputs rather than the
## filename.
director_resource_mock <- function(director, mock, defining_environment) {
  structure(list(
    director = director,
    mock = mock,
    defining_environment = defining_environment
  ), class = "director_resource_mock")
}

## Otherwise, we assume it is a mock of a file for ad hoc use of
## a parser or preprocessor.
process_resource.director_resource_mock <- function(resource, ...) {
  tower(
    virtual_check        %>>%
    modification_tracker %>>%
    dependency_tracker   %>>% 
    caching_layer        %>>%
    preprocessor         %>>%
    parser               
  )(active_resource(resource), ...)
}
