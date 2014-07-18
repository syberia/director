#' Determine whether a resource exists relative to a director object.
#'
#' @param resource character. The name of the resource.
#' @return \code{TRUE} or \code{FALSE} according as it does or does not
#'   exist.
#' @examples 
#' \dontrun{
#'   # Imagine we have a file structure:
#'   #   - foo
#'   #     - one
#'   #       - one.R
#'   #       - helper.R
#'   #     - two.R
#'   #
#'   # Then the bellow will return \code{TRUE}, \code{FALSE}, and \code{TRUE},
#'   # respectively. Note that the \code{"helper.R"} file is not considered a
#'   # resource by the director as \code{"one.R"} shares its name with its
#'   # parent directory and is considered the accessible resource.
#'
#'   d <- director('foo')
#'   d$exists('one')
#'   d$exists('one/helper')
#'   d$exists('two')
#' }
director_exists <- function(resource) {
  # Definition: idempotent resources are those that share their filename
  # with the directory they reside in.
  'Determine whether or not a resource exists in this director structure.'

  rooted_resource <- file.path(root, resource)

  # For a non-idempotent resource to exist, it must both be present as a .r
  # file *and* not be a helper to an idempotent resource.
  # TODO: (RK) Support nested idempotent resources?
  non_idempotent_exists <-
    extensionless_exists(rooted_resource) &&
    !is.idempotent_directory(dirname(rooted_resource))

  # Non-idempotence is preferred to idempotence when looking for resources.
  if (non_idempotent_exists) {
    # But we should still warn the user that there is a (now invisible to the
    # director) idempotent version of the resource.
    if (is.idempotent_directory(rooted_resource)) 
      warning("There is both a directory ", sQuote(rooted_resource), " and "
              "a file ", sQuote(paste0(rooted_resource, '.r')) " in your ",
              project_name " project. This might be confusing and cause problems.",
              call. = FALSE, immediate. = TRUE)
   
    return(TRUE)
  }

  idempotent_exists <- is.idempotent_directory(rooted_resource)

  idempotent_exists
}

