#' Fetch info about a file relative to a syberia directory.
#'
#' Some additional information will be returned. Namely, a list with
#' four keys, `current`, `cached`, `value`, and `modified`.
#' The former two will contain information relating to the current
#' and previous execution of this file, while `value` contains
#' a function that will execute the file and return its output,
#' and `modified` is a logical flag indicating whether or not the
#' file has been modified since it was last executed by Syberia.
#' 
#' Internally, the cached information about the file is stored in the
#' syberia project's registry under the key \code{resources/resource_cache}.
#'
#' @name syberia_resource
#' @param filename character. The filename of the R script to execute
#'   relative to a syberia project.
#' @param root character. The root of the syberia project in which to
#'   look for \code{filename}. The default is \code{syberia_root()}.
#' @param provides list or environment. A list or environment of values
#'   to provide to the file.
#' @param body logical. Whether or not the fetch the body of the file
#'   in the `current` and `cached` output lists.
#' @param soft logical. Whether or not to modify the cache to reflect
#'   the file modification time and other details.
#' @param ... additional arguments to pass to the \code{base::source}
#'   function that gets executed when the `value` is accessed.
#' @return a four-element list with names `current`, `cached`, `value`,
#'   and `modified`. The former two will both be two-element lists containing
#'   keys `info` and `body` (unless Syberia has never executed the file before
#'   in which case the latter will be \code{NULL}). The `info` key holds the
#'   result of calling R's built-in \code{file.info} on the file (and includes
#'   information like created at and modified at time), whereas the `body`
#'   key holds a character representation of the body of the file (useful
#'   for comparing if any actual modifications have been made).
#'
#'   The `value` key in the returned list holds a function which will
#'   execute the given file in the \code{provides} environment. A function
#'   is returned instead of the actual value to let the caller control
#'   whether or not the file is executed (e.g., based on its properties
#'   in the `current` list). Most frequently, this means consulting the
#'   `modified` key of the returned list, which will hold a logical
#'   indicating whether or not the file has been modified since last
#'   executed by Syberia (if this was never the case, `modified` will
#'   be \code{FALSE}).
syberia_resource <- function(filename, root = syberia_root(), provides = list(),
                             body = TRUE, soft = FALSE, ...) {

  if (!is.environment(provides)) {
    provides <- if (length(provides) == 0) new.env() else as.environment(provides)
    parent.env(provides) <- get_cache('runtime/current_env') %||% new.env()
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

# TODO: (RK) It would be cool to dynamically look at the memory size of the object
# in the result, and store it in the cache as well if it's small enough and the
# file has not been modified.

#' Fetch a syberia resource with modification tracking.
#'
#' Modification tracking refers to determining whether the resource has
#' changed since its last execution by syberia. This is accomplished
#' using the output of \code{syberia_resource}, which will return
#' a list that includes modification time of the last time syberia
#' encountered the resource. If a modification is spotted, the
#' Syberia cache entry `runtime/any_modified` will be set to \code{TRUE}.
#'
#' @param filename character. See the \code{filename} parameter of
#'   \code{syberia_resource}.
#' @param root character. See the \code{root} parameter of \code{syberia_resource}.
#' @param check_helpers logical. If the resource is in a directory with
#'   the same name as the resource, check all of its helpers for 
#'   modifications as well.
#' @param ... same as arguments to \code{syberia_resource}
#' @seealso \code{\link{syberia_resource}}
#' @export
syberia_resource_with_modification_tracking <- function(filename, root, check_helpers = TRUE, ...) {
  resource <- syberia_resource(filename, root, ...)
  # Store memory of this call to a stack in the cache, so we can "re-play" it
  # when a syberia model gets re-run next time. That is how we know we must, 
  # e.g., re-compile some stages because some just-in-time resources (like
  # tundra containers) were modified.
  if (identical(check_helpers, TRUE))
    syberia_stack(list(filename = normalizePath(filename), root = root))

  if (resource$current$info$mtime > resource$cached$info$mtime %||% 0)
    set_cache(TRUE, 'runtime/any_modified')
  else if (check_helpers && !identical(TRUE, get_cache('runtime/any_modified'))) {
    # The above condition says: if something is already modified,
    # no need to check again.
    filename <- normalizePath(filename)
    resource_dir <- dirname(filename)

    # If the resource is in its own subdirectory, check all helpers files for
    # modification time as well.
    resource_has_helpers <- basename(resource_dir) ==
        substring(tmp <- basename(filename), 1, nchar(tmp) - 2)
    if (resource_has_helpers) {
      helper_files <- list.files(resource_dir, recursive = TRUE)
      # Trigger syberia_resource_with_modification_tracking to update whether
      # or not any helper files were modified.
      helper_files <- setdiff(helper_files, basename(filename))
      for (file in helper_files) syberia_resource_with_modification_tracking(
        file.path(resource_dir, file), root, body = FALSE, check_helpers = FALSE)
    }
  }
  resource
}

#' A helper interface for a stack of syberia resources.
#'
#' This function is meant to be used by syberia for pushing and popping
#' resource reference onto a stack, e.g., to keep track of dependencies
#' for a given syberia model.
#'
#' This function is meant to be used as follows. Calling \code{syberia_stack()}
#' pops the latest value off the stack. If no value exists, the stack gets
#' created in the syberiaStructure cache with key 'resource_stack' if it
#' is not present, or otherwise returns NULL.
#' 
#' If a single unnamed value is passed, like \code{syberia_stack("foo")},
#' it is pushed onto the stack (if the stack does not exist in the cache,
#' it gets created extemporaneously).
#'
#' @param value ANY. Push a value onto the stac.
#' @param all logical. Whether or not to pop and return all values from
#'   the stack. The default is \code{FALSE}.
#' @return the popped value(s)
#' @examples
#' \dontrun{
#'   syberia_stack(all = TRUE) # Create a new stack
#'   lapply(1:5, syberia_stack) # Push 1 through 5 onto the stack
#'   syberia_stack(all = TRUE) # Clear the stack and return as.list(1:5)
#' }
syberia_stack <- function(value, all = FALSE) {
  if (!is.element('resource_stack', get_cache_names()))
    set_cache(stack(), 'resource_stack')
  resource_stack <- get_cache('resource_stack')
  if (!missing(value)) resource_stack$push(value)
  else if (resource_stack$empty()) NULL
  else if (identical(all, FALSE)) resource_stack$pop()
  else resource_stack$pop_all()
}

