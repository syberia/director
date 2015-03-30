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
#' @param soft logical. Whether or not to replaced the cached info about the
#'    resource.
resource_initialize <- function(director, name,
                                provided_environment = new.env(),
                                defining_environment = parent.frame(),
                                helper = FALSE, tracking = TRUE, soft = FALSE) {

  enforce_type(director, "director", "directorResource$initialize")

  director <<- director
  name     <<- resource_name(name)
  compiled <<- FALSE
  .value   <<- structure(NULL, class = "uninitializedField")
  defining_environment <<- defining_environment

  ## The default value of `defining_environment` is `parent.frame()`, and we
  ## mean the calling environment of *this* function, not any helper functions
  ## that use `defining_environment`.

  ## We use director$exists to determine whether `name` corresponds to a
  ## resource. If `helper` is `TRUE`, we look through helper .R files as well.
  if (!director$exists(name, helper = isTRUE(helper))) {
    initialize_virtual(name, defining_environment)
  } else {
    ## Convert from a list to an environment, if necessary, and set the parent
    ## environment to `defining_environment`.
    provided_environment <<-
      sanitize_provided_environment(provided_environment, defining_environment)

    initialize_real(provided_environment, isTRUE(helper),
                    isTRUE(tracking), isTRUE(soft))
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

  if (!director$has_preprocessor(name)) { # No preprocessor exists
    stop(sprintf("Cannot find resource %s, in%s project %s.",
      sQuote(crayon::red(name)),
      if (nzchar(.project_name)) paste0(" ", director$project_name()) else "",
      sQuote(crayon::blue(director$root()))))
  }

  ## If there is no such file in the project but a preprocessor exists,
  ## we let the preprocessor handle it. This is useful for "virtual"
  ## resources that do not correspond to a file and are built some other
  ## way (e.g., from a database, external web resource, etc.).
  current <<- cached <<- NULL
  modified <<- TRUE
  source_args <<- list(local = new.env(parent = defining_environment))
}

`set_filename!` <- function(helper) {
  filename <<- director$filename(name, absolute = TRUE, check.exists = FALSE,
                                helper = helper)
}

`set_details!` <- function(soft) {
  resource_info   <- if (file.exists(filename)) file.info(filename)
  cache_key       <- resource_cache_key(name)
  cached          <<- director$cache$get(cache_key)

  current <<- list(
    info         = resource_info,
    dependencies = cached$dependencies
  )

  if (is.element('value', names(cached))) {
    ## If `cached$value` was `NULL`, using 
    ## `current$value <- cached$value` would remove the
    ## element from the list instead of assigning it the value `NULL`.
    current['value'] <<- cached['value'] 
  }

  if (!soft) {
    director$cache$set(cache_key, current)
  }
}

`set_modified!` <- function(helper, tracking) {
  modified <<-
    ## If we have no file info now but we did last time this resource
    ## was parsed, the file was deleted (and thus modified).
    (is.null(current$info) && !is.null(cached)) || 
    ## If the modification timestamp has changed, the file has been
    ## modified.
    (current$info$mtime > cached$info$mtime %||% 0) 

  resource_dir <- file.path(director$root(), name)
  
  ## If the resource is idempotent, we will look through the helper files
  ## to see if their modification time has changed, to mark the
  ## `modified` flag appropriately.
  if (is.idempotent_directory(resource_dir)) {
    tracking_is_on_and_resource_has_helpers <-
      tracking && !helper && !modified
      
    # Touch helper files to see if they got modified.
    helper_files <- get_helpers(resource_dir)
    for (file in helper_files) {
      helper_object <- directorResource_$new(director = director,
        name = file.path(name, file), tracking = FALSE, helper = TRUE)
      ## Even though this statement does not always run, we still
      ## bother constructing each `directorResource` object to
      ## ensure that the `cached` entry is updated for those
      ## helper files in the `director` cache.
      if (tracking_is_on_and_resource_has_helpers)
        modified <<- modified || helper_object$modified
    }
  }
}

`mark_as_dependency!` <- function() {
  if (director$tracking_dependencies()) {
    director$push_dependency(
      list(
        level    = director$nesting_level(),
        key      = name,
        resource = self
      )
    )                            
  }
}

initialize_real <- function(provided_environment, helper, tracking, soft) {
  `set_filename!`(helper)
  `set_details!`(soft)
  `set_modified!`(helper, tracking)
  source_args <<- list(filename, local = provided_environment)
  if (!helper) `mark_as_dependency!`()
}

#    current = NULL, # list or NULL
#    cached  = NULL, # list or NULL
#    modified = FALSE, # logical
#    resource_key = NULL, # character
#    source_args = NULL, # list
#    director = NULL, # director    
#    defining_environment = NULL, # environment
#    dependencies = NULL, # character
#    compiled = NULL, # logical
#    value = NULL, # ANY
