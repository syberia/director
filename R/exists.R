#' Determine whether a resource exists relative to a director object.
#'
#' @param resource character. The name of the resource.
#' @param helper logical. Whether or not to check helper existence
#'   in an idempotent resource. The default is \code{FALSE}.
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
director_exists <- function(resource, helper = FALSE) {
  # Definition: idempotent resources are those that share their filename
  # with the directory they reside in.
  "Determine whether or not a resource exists in this director structure."

  resource <- strip_r_extension(resource)

  if (basename(dirname(resource)) == basename(resource)) {
    resource <- dirname(resource)
  }

  if (isTRUE(helper))  {
    extensionless_exists(file.path(.root, resource))
  } else {
    length(.self$find(resource, method = "exact", by_mtime = FALSE)) == 1
  }
}

