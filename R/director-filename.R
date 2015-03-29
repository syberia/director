#' Convert a resource to a filename.
#'
#' @param name character. The resource name to convert to a full path.
#' @param absolute character. Whether or not to return the absolute path
#'    (i.e., prepended by the director root). The default is \code{FALSE}.
#' @param check.exists logical. Whether or not to check if the file exists.
#'    The default is \code{TRUE}. This argument should be set to false if the
#'    we are certain the file exists and can skip the check.
#' @param helper logical. Whether or not to handle helper files instead of
#'   resources. The default is \code{FALSE}.
#' @return the absolute path, relative to the director root if
#'    \code{absolute = FALSE} and an absolute path if \code{absolute = TRUE}.
director_filename <- function(name, absolute = FALSE, check.exists = TRUE,
                              helper = FALSE) {
  ## [A reference class docstring](http://stackoverflow.com/a/5931576/2540303)
  "Convert a resource name to a file name."

  if (isTRUE(check.exists) && !exists(name, helper = isTRUE(helper))) {
    stop("Cannot convert resource ", crayon::red(name), " to full file path, ",
         "as no such resource exists.")
  }

  if (isTRUE(file.info(file.path(root(), name))$isdir)) {
    file <- complete_extension(file.path(name, basename(name)), root())
  } else {
    file <- complete_extension(name, root())
  }

  if (absolute) {
    file.path(root(), file)
  } else {
    file
  }
}