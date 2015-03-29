#' @include director-exists.R director-filename.R director-find.R
#' @include director-initialize.R director-parsers.R
#' @include director-preprocessors.R director-resource.R
#' @include utils.R
#' @include resource-initialize.R
NULL

# NOTE: This file is prepended with "zzz_" to ensure other files are parsed
# first. This is because the DESCRIPTION file's "Collate" directive is obsolete.

#' A director is an \link[=https://github.com/wch/R6]{R6 class} responsible for
#' a collection of file-traversal related responsibilities.
#'
#' @details Throughout, a "resource" refers to an R script
#'   with possible helper functions. A resource with helpers is identified
#'   by the following heuristic: if a filename sans extension is identical
#'   to its directory name, it is considered the primary accessible "resource"
#'   with any accompanying helpers (files in the same directory) natively
#'   invisible to the directory. For example, a file called \code{"foo.R"}
#'   residing in directory \code{"foo"} will be accessible from the director
#'   object using \code{director_object$resource('foo')}, but any other R scripts
#'   in the \code{"foo"} directory will not be visible to the director object.
#' 
#'   With this definition of resource, a director manages all of the following
#'   relative to a root directory. 
#'
#' \describe{
#'   \item{"Loading"}{Grabbing resources relative to the root directory
#'      using the \code{resource} method. This also provides information
#'      about the last time the resource was grabbed (modification time,
#'      previous body).}
#'   \item{"Tracking"}{Besides tracking information about the loaded resource
#'      (and any previous versions of the loaded resource), the director also
#'      maintains a stack of resources that have been loaded so far. This allows
#'      one to, for example, force clear the stack, execute some code, and have
#'      a list of resources that were relevant to the computation. The current
#'      stack is available with \code{director_object$stack(all = TRUE)}, which
#'      will also clear it.}
#'   \item{"Parsers"}{Some resources are not intended to be merely executed,
#'      but also parsed. For example, if we define several functions in an
#'      R script, we have access to those functions if we had sourced it
#'      with \code{base::source(script, local = some_environment)} as they
#'      are now present in \code{some_environment}. A parser is a wrapper
#'      around loading of resources that allows one to do some further
#'      computation with this information. For example, we may define
#'      a \code{read} and \code{write} function, but parse this information
#'      into some IO object when fetching the resource. Thus, a resource
#'      can function as inputs to some parser that produces some final
#'      resource object. Parsers can be defined for full directories or
#'      specific files.}
#' }
#'
#' @rdname director
director_ <- R6Class("director",
  private = list(
    .root           = NULL, # character
    .project_name   = NULL, # character
    .resource_cache = list(), # list
    .registry       = NULL, # registry
    .dependency_nesting_level = 0, # integer
    .parsers        = list(), # list
    .preprocessors  = list(), # list
    .cached_resources = list() # character
  ),
  public = list(                    
    # Members
    dependency_stack = NULL, # stack
    cache            = NULL,

    # Methods
    initialize = initialize,
    exists     = director_exists,
    resource   = resource,
    virtual_resource = virtual_resource,
    register_parser = register_parser,
    register_preprocessor = register_preprocessor,
    has_preprocessor = has_preprocessor,
    find       = director_find,

    match_preprocessor = function(resource_name) {
      Find(function(x) substring(resource_name, 1, nchar(x)) == x, names(.preprocessors))
    },
    match_parser = function(resource_name) {
      Find(function(x) substring(resource_name, 1, nchar(x)) == x, names(.parsers))
    },
    preprocessor = function(x) .preprocessors[[x]],
    parser = function(x) .parsers[[x]],
    cached_resources = function() .cached_resources,

    tracking_dependencies = function() { .dependency_nesting_level > 0L },
    clear_resource_stack = function() { if (.dependency_nesting_level == 0) dependency_stack$clear() },
    increment_nesting_level = function() { .dependency_nesting_level <<- .dependency_nesting_level + 1L },
    decrement_nesting_level = function() { .dependency_nesting_level <<- .dependency_nesting_level - 1L },
    nesting_level = function() { .dependency_nesting_level },

    root         = function() { .root },
    project_name = function() { .project_name },
    absolute   = function(x) { file.path(root(), x) },
    show       = function() {
      cat(sep = '', "Director object",
          if (isTRUE(nzchar(.root))) paste0(" monitoring ", sQuote(.root),
            if (isTRUE(nzchar(.project_name))) paste(" for", .project_name, "project")),
          ".\n")
    },
    filename  = director_filename
  )
)

director <- structure(
  function(...) { director_$new(...) },
  class = "director_"
)

`$.director_` <- function(...) {
  stopifnot(identical(..2, "new"))
  ..1
}

#' @docType function
#' @name director
#' @export
NULL

#' Whether or not an object is a director.
#' @param x ANY.
#' @export
is.director <- function(x) is(x, 'director')

