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

  rooted_resource <- strip_r_extension(resource)
  if (basename(dirname(rooted_resource)) == basename(rooted_resource))
    rooted_resource <- dirname(rooted_resource)

  return(
    if (isTRUE(helper)) extensionless_exists(file.path(.root, rooted_resource))
    else length(.self$find(rooted_resource, method = "exact", by_mtime = FALSE)) == 1
  )

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
      warning("There is both a directory ", sQuote(resource), " and ",
              "a file ", sQuote(paste0(resource, '.r')), " in your ",
              .project_name, " project. This might be confusing and cause problems.",
              call. = FALSE, immediate. = TRUE)
   
    return(TRUE)
  }

  # If it is not an idempotent resource, check if either it is an idempotent
  # directory or it is the idempotent file itself.
  idempotent_exists <-
    if (file.exists(rooted_resource) && file.info(rooted_resource)$isdir)
      is.idempotent_directory(rooted_resource)
    else if (basename(rooted_resource) == basename(dirname(rooted_resource)))
      extensionless_exists(rooted_resource)
    else FALSE

  idempotent_exists
}

