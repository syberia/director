#' Initialize a director object.
#'
#' @param root character. The root directory for the director.
#' @param project_name character. The name of the director project. Useful for
#'    error messages. For example, if a resource is not found, an error message
#'    to the effect of "no resource foo found in your \code{project_name}
#'    project" will be displayed.
#' @return a \code{director} reference class object.
#' @examples
#' \dontrun{
#' director(tempdir())
#' director(tempdir(), "my project") # Error messages on using the director's
#'                                   # methods will now usually result in
#'                                   # the ending "in project 'my project'".
#' }
initialize <- function(root, project_name = '') {
  ## Reference class objects are sometimes initialized on package install, but
  ## no arguments are passed! We let it through to avoid installation problems.
  if (missing(root)) return() 

  enforce_type(project_name, "character", "director$new")

  if (length(project_name) != 1) {
    stop("Project name for ", crayon::blue("director$new"), " call must ",
         "be a scalar character, but has length ",
         crayon::red(as.character(length(project_name))), ".")
  }

  if (!file.exists(root)) {
    stop("Cannot create a director for ", crayon::red(root), " as that directory ",
          "does not exist.")
  }

  if (!file.info(root)$isdir) {
    stop("Cannot create a director for ", crayon::red(root), " as that is a file ",
          "and not a directory.")
  }

  # Set R6 fields.
  self$.dependency_nesting_level <<- 0L
  self$.root             <<- normalizePath(root)
  # TODO: (RK) Customize location of the registry: https://github.com/robertzk/director/issues/20
  self$.registry         <<- registry(file.path(self$.root, '.registry'))
  self$.project_name     <<- project_name
  self$dependency_stack  <<- shtack$new()

  self$.resource_cache   <<- list()
  self$.parsers          <<- list()
  self$.preprocessors    <<- list()
  self$.cached_resources <<- list()
  self$cache             <<- simple_cache()

  ## We need a unique identifier for each director object, so we can keep track
  ## of state separately in the `active_resource` helper.
  .director_env$count <- (.director_env$count %||% 0) + 1
  self$.id <<- .director_env$count
}

