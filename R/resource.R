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
#' `value`, and `modified` if it finds the given resource, or error
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
#'   to the resource. The default is nothing, i.e., \code{list()}. Note that
#'   \code{provides} will be coerced to an environment, and its parent 
#'   environment will be set to \code{parent.env(topenv())} to prevent
#'   access to global variables (and encourage modularity and lack of side
#'   effects. There should always be a way to write your code without them).
#' @param body logical. Whether or not the fetch the body of the resource
#'   in the `current` and `cached` output lists.
#' @param soft logical. Whether or not to modify the cache to reflect
#'   the resource modification time and other details.
#' @param ... additional arguments to pass to the \code{base::source}
#'   function that gets executed when the `value` is accessed.
#' @param tracking logical. Whether or not to perform modification tracking
#'   by pushing accessed resources to the director's stack.
#' @param check.helpers logical. If \code{TRUE}, the resource's helpers
#'   (assuming it is an idempotent resource -- that is, a resource whose
#'   parent directory has the same name as the resource). The default is
#'   \code{TRUE}. If \code{FALSE}, the \code{name} parameter will not be
#'   converted into a resource name.
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
                     tracking = FALSE, check.helpers = TRUE) {

  if (!is.environment(provides)) {
    provides <- if (length(provides) == 0) new.env() else as.environment(provides)
    parent.env(provides) <- parent.env(topenv())
    # Do not allow access to the global environment since resources should be self-contained.
  }

  if (!exists(name)) # Note we are using director$exists not base::exists
    stop("Cannot find resource ", colourise(sQuote(name), 'red'), " in ",
         .project_name, " project ", colourise(sQuote(.root), 'blue'), ".")

  if (isTRUE(check.helpers))
    filename <- .filename(name, FALSE, FALSE) # Convert resource to filename.
  resource_info   <- if (file.exists(filename)) file.info(filename)
  resource_key    <- strip_root(.root, resource_name(filename))
  resource_cache_key <- file.path('resource_cache', digest(resource_key))
  cached_details  <- .cache[[resource_cache_key]]
  current_details <- list(info = resource_info)

  if (body) current_details$body <- paste(readLines(filename), collapse = "\n")

  if (identical(soft, FALSE)) .cache[[resource_cache_key]] <<- current_details

  source_args <- append(list(filename, local = provides), list(...))
  # TODO: (RK) Check if `local` is an environment in case user overwrote.
  director_obj <- .self
  value <- function() {
    # TODO: (RK) Preprocess the resource?
    # TODO: (RK) Copy env from provides to prevent double writing?
    value <- do.call(base::source, source_args)$value
    value <- director_obj$compile(value, source_args$local, resource_key,
                                  tracking = tracking)
    value
  }

  modified <-
    (is.null(resource_info) && !is.null(cached_details)) || # file was deleted
    (resource_info$mtime > cached_details$info$mtime %||% 0) # file was changed

  tracking_is_on_and_resource_has_helpers <-
    isTRUE(tracking) && isTRUE(check.helpers) &&
    !isTRUE(modified) && # No point in checking modifications in helpers otherwise
    is.idempotent_directory(resource_dir <- file.path(.root, resource_key)) &&
    
  if (tracking_is_on_and_resource_has_helpers) {
    helper_files <- list.files(resource_dir) # TODO: (RK) Recursive helpers?
    same_file <- which(sapply(helper_files, 
      function(f) strip_extension(f) == basename(resource_key)))
    helper_files <- helper_files[-same_file]
    browser()
    for (file in helper_files) modified <- modified ||
        resource(file.path(resource_dir, file), body = FALSE,
                 tracking = FALSE, check.helpers = FALSE)$modified
  }

  output <- list(current = current_details, cached = cached_details,
       value = value, modified = modified)
  if (isTRUE(.track)) .stack$push(output)
  output
}

