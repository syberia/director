#' Initialize a director resource.
#'
#' @seealso \code{directorResource}
#' @param director director. The director object to which this resource 
#'    belongs.
#' @param name character. The name of the resource. For example, if
#'    we have a standalone resource \code{"linear_model.R"}, then
#'    its resource name would be \code{"linear_model"}. If we have
#'    an idempotent resource \code{"ensemble_model"} (a directory),
#'    its name would be \code{"ensemble_model"}.
#' @param provided_environment environment. An environment with values that
#'    should always be provided to the resource upon sourcing. For example,
#'    if you overwrote the formula operator \code{~} to provide some custom
#'    syntax in your R script, you can put it in an environment and it will
#'    be available when sourcing the resource. The default is a new
#'    environment.
#' @param defining_environment environment. This will become the parent
#'    environment of the \code{provided_environment}. If, for example,
#'    the resource is created from a package method and uses functions
#'    from within that package's Imports, the \code{defining_environment}
#'    will ensure that compiling the resource will find those methods.
#' @param helper logical. Whether or not to treat helper files as resources.
#'    This is used internally to keep track of modifications.
#' @param tracking logical. Whether or not to perform dependency tracking.
#'    If \code{TRUE} (the default), then \code{this_resource$modified()} 
#'    will return \code{TRUE} if any of its dependencies have been modified.
#'
#'    Another resource is a dependency if it was compiled during the compilation
#'    of this resource.
resource_initialize <- function(director, name,
                                provided_environment = new.env(),
                                defining_environment = parent.frame(),
                                helper = FALSE, tracking = TRUE) {

  ## This does not hurt unless someone names their file "foo.R.R",
  ## and it would be inconvenient to the user if we did not strip the extension.
  name <- strip_r_extension(name)

  ## The default value of `defining_environment` is `parent.frame()`, and we
  ## mean the calling environment of *this* function, not any helper functions
  ## that use `defining_environment`.
  force(defining_environment)

  ## We use director$exists to determine whether `name` corresponds to a
  ## resource. If `helper` is `TRUE`, we look through helper .R files as well.
  if (!exists(name, helper = isTRUE(helper))) {
    initialize_virtual(name, defining_environment)
  } else {
    ## Convert from a list to an environment, if necessary, and set the parent
    ## environment to `defining_environment`.
    provided_environment <-
      sanitize_provided_environment(provided_environment, defining_environment)

    initialize_real(name, provided_environment, helper, tracking)
  }
}

sanitize_provided_environment <- function(provides, defining_environment) {
  if (!is.environment(provides)) {
    if (length(provides) == 0) provides <- new.env(parent = defining_environment)
    else provides <- list2env(provides, parent = defining_environment)
  }

  if (base::exists('..director_inject', envir = parent.env(provides), inherits = FALSE)) {
    # TODO: (RK) Calling parent.env here twice since we're doing environment injection
    # in resource$compile - is there a better way?
    parent.env(parent.env(provides)) <- parent.env(topenv())
  } else parent.env(provides) <- parent.env(topenv(provides))

  # Do not allow access to the global environment since resources should be self-contained.
  provides
}

virtual_resource <- function(name, defining_environment) {
  # TODO: (RK) Should assuming virtual resource be the right behavior here?

  if (!has_preprocessor(name)) { # No preprocessor exists
    stop(sprintf("Cannot find resource %s, in%s project %s.",
      sQuote(crayon::red(name)),
      if (nzchar(.project_name)) paste0(" ", .project_name) else "",
      sQuote(crayon::blue(.root))))
  }

  ## If there is no such file in the project but a preprocessor exists,
  ## we let the preprocessor handle it. This is useful for "virtual"
  ## resources that do not correspond to a file and are built some other
  ## way (e.g., from a database, external web resource, etc.).
  return(directorResource(current = NULL, cached = NULL,
    modified = TRUE, resource_key = name,
    source_args = list(local = new.env(parent = defining_environment)),
    director = self, defining_environment = defining_environment))
}

initialize_real <- function(name, provided_environment, helper, tracking) {
  filename  <- self$filename(name, absolute = TRUE, check.exists = FALSE,helper = isTRUE(helper)) # Convert resource to filename.
  resource_info   <- if (file.exists(filename)) file.info(filename)
  resource_key    <- strip_root(.root, resource_name(filename))
  cache_key       <- resource_cache_key(resource_key)
  cached_details  <- cache$get(cache_key)
  current_details <- list(info = resource_info)
  current_details$dependencies <- cached_details$dependencies
  if (is.element('value', names(cached_details)))
    current_details['value'] <- cached_details['value'] # (avoid NULL problems)

  if (isTRUE(body)) current_details$body <-
    paste(readLines(filename, warn = FALSE), collapse = "\n")

  if (identical(soft, FALSE)) cache$set(cache_key, current_details)

  source_args <- list(filename, local = provides)
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
                         tracking = FALSE, helper = TRUE)
                         #defining_environment = parent.frame())
      if (tracking_is_on_and_resource_has_helpers)
        modified <- modified || helper_object$modified
    }
  }

  # TODO: (RK) Finer control over defining environment.
  output <- directorResource(current = current_details, cached = cached_details,
       modified = modified, resource_key = resource_key,
       source_args = source_args, director = self,
       defining_environment = parent.frame()) 

  if (.dependency_nesting_level > 0 && !isTRUE(helper))
    dependency_stack$push(list(level = .dependency_nesting_level,
                     key = resource_key,
                     resource = output))
  output
}
