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
  .dependency_nesting_level <<- 0L
  .root             <<- normalizePath(root)
  # TODO: (RK) Customize location of the registry: https://github.com/robertzk/director/issues/20
  .registry         <<- registry(file.path(.root, '.registry'))
  .project_name     <<- project_name
  dependency_stack  <<- shtack$new()

  .resource_cache   <<- list()
  .parsers          <<- list()
  .preprocessors    <<- list()
  .cached_resources <<- list()
  cache             <<- simple_cache()
}

