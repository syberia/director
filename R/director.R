#' @include director-exists.R director-filename.R director-find.R
#' @include director-initialize.R director-parsers.R
#' @include director-preprocessors.R director-resource.R
#' @include utils.R
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
#' @docType class
#' @rdname director
#' @format NULL
director_ <- R6::R6Class("director",
  portable = TRUE,                        
  private = list(
  ),
  public = list(                    
    .root           = NULL, # character
    .project_name   = NULL, # character
    .resource_cache = list(), # list
    .registry       = NULL, # registry
    .dependency_nesting_level = 0, # integer
    .parsers        = list(), # list
    .preprocessors  = list(), # list
    .cached_resources = list(), # character
    .id             = NULL, # integer
    # Members
    dependency_stack = NULL, # stack
    cache            = NULL,

    # Methods
    initialize = initialize,
    exists     = director_exists,
    resource   = resource,
    #virtual_resource = virtual_resource,
    register_parser = register_parser,
    register_preprocessor = register_preprocessor,
    has_preprocessor = has_preprocessor,
    find       = director_find,

    match_preprocessor = function(resource_name) {
      Find(function(x) substring(resource_name, 1, nchar(x)) == x,
           substring(names(self$.preprocessors), 2))
    },
    match_parser = function(resource_name) {
      Find(function(x) substring(resource_name, 1, nchar(x)) == x,
           substring(names(self$.parsers), 2))
    },
    preprocessor = function(x) self$.preprocessors[[paste0("/", x)]],
    parser = function(x) self$.parsers[[paste0("/", x)]],
    cached_resources = function() self$.cached_resources,

    cache_get = function(k) { self$cache$get(k) },
    cache_set = function(k, v) { self$cache$set(k, v) },
    cache_exists = function(k) { self$cache$exists(k) },

    tracking_dependencies = function() { self$.dependency_nesting_level > 0L },
    clear_resource_stack = function() { if (self$.dependency_nesting_level == 0) self$dependency_stack$clear() },
    increment_nesting_level = function() { self$.dependency_nesting_level <<- self$.dependency_nesting_level + 1L },
    decrement_nesting_level = function() { self$.dependency_nesting_level <<- self$.dependency_nesting_level - 1L },
    nesting_level = function() { self$.dependency_nesting_level },
    push_dependency = function(dependency) {
      self$dependency_stack$push(dependency)
    },
    peek_dependency_stack = function(...) { self$dependency_stack$peek(...) },
    pop_dependency_stack = function() { self$dependency_stack$pop() },
    empty_dependency_stack = function() { self$dependency_stack$empty() },

    root         = function() { self$.root },
    project_name = function() { self$.project_name },
    absolute   = function(x) { file.path(self$root(), x) },
    show       = function() {
      cat(sep = '', "Director object",
          if (isTRUE(nzchar(self$.root))) paste0(" monitoring ", sQuote(self$.root),
            if (isTRUE(nzchar(self$.project_name))) paste(" for", self$.project_name, "project")),
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

#' @export
print.director <- function(x, ...) {
  x$show()
}

#' @docType function
#' @name director
#' @export
NULL

#' Whether or not an object is a director.
#' @param x ANY.
#' @export
is.director <- function(x) is(x, 'director')

#' If the parser and preprocessor for a path is the same, the user has probably made a mistake.
#'
#' @param director director. The director object.
#' @param path character. The path to check for whether the preprocessor and
#'   parser are identical.
#'
#' @return Nothing, but issue a warning in red crayon if they are identical,
#'   since it likely means the user forgot to specify a parser.
check_if_parser_and_preprocessor_are_identical <- function(director, path) {
  has_same_body <- function(fn1, fn2) {
    is.function(fn1) && is.function(fn2) && 
      isTRUE(all.equal(body(fn1), body(fn2)))
  }
  if (has_same_body(director$.parsers[[paste0("/", path)]],
                    director$.preprocessors[[paste0("/", path)]])) {
    warning(crayon::red("The path at ", sQuote(path), " has the same ",
                        "preprocessor and parser -- are you sure you ",
                        "included a parser?"))
  }
}

