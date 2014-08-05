#' Initialize a director object.
#'
#' @param root character. The root directory for the director.
#' @param project_name character. The name of the director project. Useful for
#'    error messages. For example, if a resource is not found, an error message
#'    to the effect of "no resource foo found in your \code{project_name}
#'    project" will be displayed.
initialize <- function(root, project_name = root) {
  if (missing(root)) return()
  if (!file.exists(root))
    stop("Cannot create a director for ", sQuote(root), " as that directory ",
          "does not exist.")

  if (!file.info(root)$isdir)
    stop("Cannot create a director for ", sQuote(root), " as that is a file ",
          "and not a directory.")

  # Set reference class fields.
  .dependency_nesting_level <<- 0L
  .root         <<- normalizePath(root)
  .registry     <<- registry(file.path(.root, '.registry'))
  .project_name <<- project_name
}

