#' Fetch a resource relative to a director object.
#'
#' Resources are R scripts that optionally have a "parser" attached
#' which takes the result of executing the file, including all of its local
#' variables, and does some additional computation. This is useful if,
#' for example, you are trying to define a standard format for creating a
#' reference class object by specifying some inputs, but want to make it
#' easy to provide those inputs by users.
#' 
#' This method will return a \code{directorResource} object that represents
#' that particular R script. A resource can have a \code{\link[=register_preprocessor]{preprocessor}}
#' and a \code{link[=register_parser]{parser}} attached to it.
#'
#' The former determines how to source the R file. For example, if you need
#' to inject additional variables prior to sourcing it, you can do so
#' from the preprocessor.
#' 
#' The parser determines what to do with the R file after sourcing it.
#' It can tell what the dependencies of the file are (i.e., what other
#' resources were used when sourcing it), and whether or not it was modified
#' (i.e., whether the R file or any of its dependencies were modified).
#' 
#' Together, a preprocessor, parser, and source file compose a resource.
#'
#' @param name character. The name of the resource (i.e. R script) relative
#'   to the root of the director object.
#' @param provides list or environment. A list or environment of values to provide
#'   to the resource. The default is nothing, i.e., \code{list()}. Note that
#'   \code{provides} will be coerced to an environment, and its parent 
#'   environment will be set to \code{parent.env(topenv())} to prevent
#'   access to global variables (and encourage modularity and lack of side
#'   effects. There should always be a way to write your code without them).
#' @param body logical. Whether or not to fetch the body of the resource.
#' @param soft logical. Whether or not to modify the cache to reflect
#'   the resource modification time and other details.
#' @param ... additional arguments to pass to the \code{base::source}
#'   function that gets executed when the `value` is accessed.
#' @param tracking logical. Whether or not to perform modification tracking
#'   by pushing accessed resources to the director's stack.
#' @param helper logical. If \code{TRUE}, allow processing of helper files.
# TODO: (RK) Explain idempotence and helpers more: https://github.com/robertzk/director/issues/23
#'   If a file shares its name with the parent directory (e.g., "foo"
#'   and "foo/foo.R"), it is called an idempotent resource. Any other files
#'   in the same directory as the idempotence resource, besides the file
#'   itself, are called helper files, and are usually invisible to the
#'   director object (e.g., "foo/other.R" if "foo/foo.R" exists).
#'
#'   If \code{helper = TRUE}, these will temporarily be treated as a
#'   resource so that we can track whether they were modified and re-use
#'   other \code{directorResource} features. By default, \code{helper = FALSE}.
#' @return A \code{\link{directorResource}} object.
resource <- function(name, provides = list(), body = TRUE, soft = FALSE, ...,
                     tracking = TRUE, helper = FALSE) {

  name <- strip_r_extension(name)

  if (!is.environment(provides)) {
    provides <-
      if (length(provides) == 0) new.env(parent = parent.frame())
      else {
        env <- as.environment(provides)
        parent.env(env) <- parent.frame()
        env
      }

    if (base::exists('..director_inject', envir = parent.env(provides), inherits = FALSE)) {
      # TODO: (RK) Calling parent.env here twice since we're doing environment injection
      # in resource$compile - is there a better way?
      parent.env(parent.env(provides)) <- parent.env(topenv())
    } else parent.env(provides) <- parent.env(topenv(provides))
    # Do not allow access to the global environment since resources should be self-contained.
  }

  # Note below we are using director$exists not base::exists
  if (!exists(name, helper = isTRUE(helper))) {
    # TODO: (RK) Should assuming virtual resource be the right behavior here?

    if (!has_preprocessor(name)) { # No preprocessor exists
      stop("Cannot find resource ", crayon::red(sQuote(name)), " in ",
           .project_name, " project ", crayon::blue(sQuote(.root)), ".")
    }

    # If this resource does not exist, let the preprocessor handle it instead.
    return(directorResource(current = NULL, cached = NULL,
      modified = TRUE, resource_key = name,
      source_args = list(local = new.env(parent = parent.frame())), director = .self,
      defining_environment = parent.frame()))
  }

  filename        <- .self$filename(name, absolute = TRUE, check.exists = FALSE, helper = isTRUE(helper)) # Convert resource to filename.
  resource_info   <- if (file.exists(filename)) file.info(filename)
  resource_key    <- strip_root(.root, resource_name(filename))
  cache_key       <- resource_cache_key(resource_key)
  cached_details  <- .cache[[cache_key]]
  current_details <- list(info = resource_info)
  current_details$dependencies <- cached_details$dependencies
  if (is.element('value', names(cached_details)))
    current_details['value'] <- cached_details['value'] # (avoid NULL problems)

  if (isTRUE(body)) current_details$body <-
    paste(readLines(filename, warn = FALSE), collapse = "\n")

  if (identical(soft, FALSE)) .cache[[cache_key]] <<- current_details

  source_args <- append(list(filename, local = provides), list(...))
  # TODO: (RK) Check if `local` is an environment in case user overwrote.

  modified <-
    (is.null(resource_info) && !is.null(cached_details)) || # file was deleted
    (resource_info$mtime > cached_details$info$mtime %||% 0) # file was changed

  resource_dir <- file.path(.root, resource_key)
  
  if (is.idempotent_directory(resource_dir)) {
    tracking_is_on_and_resource_has_helpers <-
      isTRUE(tracking) && !isTRUE(helper) &&
      !isTRUE(modified) # No point in checking modifications in helpers otherwise
      
    # Touch helper files to see if they got modified.
    helper_files <- get_helpers(resource_dir)
    for (file in helper_files) {
      helper_object <- resource(file.path(resource_key, file), body = FALSE,
                         tracking = FALSE, helper = TRUE,
                         defining_environment = parent.frame())
      if (tracking_is_on_and_resource_has_helpers)
        modified <- modified || helper_object$modified
    }
  }

  # TODO: (RK) Finer control over defining environment.
  output <- directorResource(current = current_details, cached = cached_details,
       modified = modified, resource_key = resource_key,
       source_args = source_args, director = .self,
       defining_environment = parent.frame()) 

  if (.dependency_nesting_level > 0 && !isTRUE(helper))
    .stack$push(list(level = .dependency_nesting_level,
                     key = resource_key,
                     resource = output))
  output
}


