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

  non_idempotent_exists <- 
    file.exists(tmp <- file.path(root, paste0(resource, '.r'))) || 
    file.exists(tmp <- file.path(root, paste0(resource, '.R')))

  #if (is.idempotent_directory(

  if (non_idempotent_exists) {
    # TODO: (RK) Should we warn if an equivalent idempotent version exists?
    return(TRUE)
  }


  #idempotent_exists <-

  #file.exists(file.path(root, paste0(resource, '.R'))) || 
}

