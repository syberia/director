#' Initialize a director object.
#'
#' @param root character. The root directory for the director.
#' @param project_name character. The name of the director project. Useful for
#'    error messages. For example, if a resource is not found, an error message
#'    to the effect of "no resource foo found in your \code{project_name}
#'    project" will be displayed.
initialize <- function(root, project_name = '') {
  root         <<- root
  project_name <<- project_name
}

