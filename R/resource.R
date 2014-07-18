#' Fetch a resource relative to a director object.
#'
#' Resources are R scripts that optionally have a "parser" attached
#' which takes the result of executing the file, including all of its local
#' variables, and does some additional computation. This is useful if,
#' for example, you are trying to define a standard format for creating a
#' reference class object by specifying some inputs, but want to make it
#' easy to provide those inputs by users.
#' 
#' This method will return a list containing four keys, `current`, `cached`,
#' `value`, and `modified` if it finds the given resource, or \code{FALSE}
#' if no such resource exists. The former two keys, `current` and `cached`,
#' will contain information relating to the current and previous execution
#' of this resource, while `value` contains a function that will execute the
#' file and return its output, and `modified` is a logical flag indicating
#' whether or not the file has been modified since it was last executed by
#' the director.
#'
#' @param name character. The name of the resource (i.e. R script) relative
#'   to the root of the director object.
#' @param provides list or environment. A list or environment of values to provide
#'   to the resource. The default is nothing, i.e., \code{list()}.
#' @param body logical. Whether or not the fetch the body of the resource
#'   in the `current` and `cached` output lists.
#' @param soft logical. Whether or not to modify the cache to reflect
#'   the resource modification time and other details.
#' @param ... additional arguments to pass to the \code{base::source}
#'   function that gets executed when the `value` is accessed.
#' @param tracking logical. Whether or not to perform modification tracking
#'   by pushing accessed resources to the director's stack.
#' @return a four-element list with names `current`, `cached`, `value`,
#'   and `modified`. The former two will both be two-element lists containing
#'   keys `info` and `body` (unless director has never executed the resource before
#'   in which case the latter will be \code{NULL}). The `info` key holds the
#'   result of calling R's built-in \code{file.info} on the resource (and includes
#'   information like created at and modified at time), whereas the `body`
#'   key holds a character representation of the body of the resource (useful
#'   for comparing if any actual modifications have been made).
#'
#'   The `value` key in the returned list holds a function which will
#'   execute the given resource in the \code{provides} environment. A function
#'   is returned instead of the actual value to let the caller control
#'   whether or not the resource is executed (e.g., based on its properties
#'   in the `current` list). Most frequently, this means consulting the
#'   `modified` key of the returned list, which will hold a logical
#'   indicating whether or not the resource has been modified since last
#'   executed by the director (if this was never the case, `modified` will
#'   be \code{FALSE}).
resource <- function(name, provides = list(), body = TRUE, soft = FALSE, ...,
                     tracking = FALSE) {
  if (!is.environment(provides)) {
    provides <- if (length(provides) == 0) new.env() else as.environment(provides)
    parent.env(provides) <- parent.env(topenv())
    # Do not allow access to the global environment since resources should be self-contained.
  }

  resource_info <- if (file.exists(filename)) file.info(filename)
  if (is.null(resource_info) || resource_info$isdir) {
    base <- if (resource_info$isdir) filename else dirname(filename)
    resource_object <- syberia_objects(pattern = basename(filename),
                                       base = base, fixed = TRUE)
    filename <- file.path(base, resource_object)
    if (length(filename) > 1) {
      stop("Multiple syberia resources found: ",
           paste0(filename, collapse = ", "), call. = FALSE)
    } else if (length(filename) == 0) {
      stop("Syberia resource ", sQuote(filename), " in syberia project ",
           sQuote(root), " does not exist.", call. = FALSE)
    } 
    resource_info <- file.info(filename)
  }

  resource_cache <- .get_registry_key('resource/resource_cache', .get_registry_dir(root))
  resource_key <- function(filename, root) # Given a/b/c/d and a/b, extracts c/d
    substring(tmp <- normalizePath(filename), nchar(normalizePath(root)) + 1, nchar(tmp))
  resource_key <- resource_key(filename, root)
  cache_details <- resource_cache[[resource_key]]

  current_details <- list(info = resource_info)
  if (body) current_details$body <- paste(readLines(filename), collapse = "\n")

  resource_cache[[resource_key]] <- current_details
  if (identical(soft, FALSE))
    .set_registry_key('resource/resource_cache', resource_cache, .get_registry_dir(root))

  # TODO: (RK) For large syberia projects, maybe this should dynamically
  # switch to tracking resources using the file system rather than one big list.

  source_args <- append(list(filename, local = provides), list(...))
  value <- function() do.call(base::source, source_args)$value
  modified <- resource_info$mtime > cache_details$info$mtime %||% 0

  list(current = current_details, cached = cache_details,
       value = value, modified = modified)
}

