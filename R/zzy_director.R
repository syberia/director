# NOTE: This file is prepended with "zzz_" to ensure other files are parsed
# first. This is because the DESCRIPTION file's "Collate" directive is obsolete.

#' Convert a resource to a filename.
#'
#' @param name character. The resource name to convert to a full path.
#' @param absolute character. Whether or not to return the absolute path
#'    (i.e., prepended by the director root). The default is \code{FALSE}.
#' @param check.exists logical. Whether or not to check if the file exists.
#'    The default is \code{TRUE}. This should be primarily used if the file
#'    has already been checked for existence.
#' @param helper logical. Whether or not to handle helper files instead of resources.
#'   The default is \code{FALSE}.
#' @return the full path, relative to the director root if \code{full = FALSE}
#'    and an absolute path if \code{FULL = TRUE}.
director_.filename <- function(name, absolute = FALSE, check.exists = TRUE, helper = FALSE) {
  filename <- name
  if (isTRUE(check.exists) && !exists(filename, helper = isTRUE(helper)))
    stop("Cannot convert resource ", sQuote(filename), " to full file path, ",
         "as no such resource exists.")

  with_absolute <- function(filename) {
    filename <- gsub('//', '/', filename, fixed = TRUE)
    if (isTRUE(absolute)) file.path(.root, filename)
    else filename
  }

  # If `name` is a directory, recursively check if it's an idempotent resource.
  if (file.exists(rooted_file <- file.path(.root, filename)) &&
      file.info(rooted_file)$isdir)
    return(.self$.filename(file.path(filename, basename(rooted_file)),
                           absolute = absolute, check.exists = check.exists))

  filename <- strip_r_extension(filename)
  if (file.exists(tmp <- file.path(.root, tmp <- paste0(filename, '.r'))) ||
      file.exists(tmp <- file.path(.root, tmp <- paste0(filename, '.R'))))
    return(with_absolute(tmp))
  
  filename <- file.path(filename, dirname(filename))
  if (file.exists(tmp <- file.path(.root, paste0(filename, '.r'))) ||
      file.exists(tmp <- file.path(.root, paste0(filename, '.R'))))
    return(with_absolute(tmp))

  stop("Cannot convert resource ", sQuote(filename), " to full file path, ",
       "as no such resource exists in ", .project_name, " project ",
       sQuote(.root), ".")
}

#' A director is a reference class responsible for a collection of
#' file-traversal related responsibilities.
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
#' \itemize{
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
#' @name director
#' @rdname director
#' @export
director <- setRefClass("director",
  fields = list(.root = 'character', .project_name = 'character',
                .resource_cache = 'list', .stack = 'stack',
                .registry = 'registry', .cache = 'list',
                .dependency_nesting_level = 'integer',
                .parsers = 'list', .preprocessors = 'list', .cached_resources = 'character'),
  methods = list(
    initialize = initialize,
    exists     = director_exists,
    resource   = resource,
    compile    = compile,
    register_parser = register_parser,
    register_preprocessor = register_preprocessor,
    has_preprocessor = has_preprocessor,
    find       = director_find,

    root       = accessor_method(.root),
    show       = function() {
      cat(sep = '', "Director object",
          if (isTRUE(nzchar(.root))) paste0(" monitoring ", sQuote(.root),
            if (isTRUE(nzchar(.project_name))) paste(" for", .project_name, "project")),
          ".\n")
    },
    .filename  = director_.filename
  )
)

#' @docType function
#' @name director
#' @export
NULL

#' Whether or not an object is a director.
#' @param x ANY.
#' @export
is.director <- function(x) is(x, 'director')

