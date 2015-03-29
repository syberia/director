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
  ## [A reference class docstring](http://stackoverflow.com/a/5931576/2540303)
  "Determine whether or not a resource exists in this director structure."

  ## We want to handle inputs like "foo.R" gracefully, since there is no reason
  ## to make life painful for the user.
  resource <- strip_r_extension(resource)

  ## Since R is a language that does not have good support for mixins
  ## and hierarchical structure, we will borrow ideas from the node.js,
  ## who has a similar problem. If a file becomes too complex, we should
  ## be able to split it up into multiple pieces -- but only "export" a
  ## single thing. You can do this by turning moving your "file.R" to
  ## the directory "file" (note that the name must match) and placing
  ## your old file in "file/file.R". Any additional files in the "file"
  ## directory, like "file/helper_function.R" or "file/constants.R"
  ## will not be detectable to the director object. This allows us to
  ## follow the important developer principles of Don't Repeat Yourself
  ## and maintaining modularity without polluting what our direcotr sees.
  ##
  ## Idempotence only applies to files in the same directory. If you have
  ## files "foo/bar/bar.R", "foo/bar/baz.R", and "foo/bar/bux/bux.R", then
  ## "foo/bar/bux" is considered another director resource. This allows
  ## for hierarchical resource structures that can still maintain their
  ## respective helpers (although it may take a little bit of getting used to).
  if (basename(dirname(resource)) == basename(resource)) {
    resource <- dirname(resource)
  }

  ## It is always preferable to wrap logical conditions with `isTRUE`.
  ## We will receive a warning if `length(helper) != 1` and we will just
  ## straight up error if `helper` is not a logical vector!
  if (isTRUE(helper))  {
    ## `absolute` is a method on the director class.
    extensionless_exists(absolute(resource))
  } else {
    ## If the director `find` method returns an exact match for this resource
    ## (i.e., `find("foo")` is `"foo"`), the resource exists and we're in
    ## business!
    length(find(resource, method = "exact", by_mtime = FALSE)) == 1
  }
}

