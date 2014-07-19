`%||%` <- function(x, y) if (is.null(x)) y else x

#' Attempt to memoize a function using the memoise package.
#' 
#' This function will load the \code{memoise} package if it is
#' available, or do nothing otherwise.
#'
#' (Blame Hadley for the spelling of memoise.)
#'
#' @param fn function. The function to memoize.
#' @return nothing, but \code{try_memoize} will use non-standard
#'   evaluation to memoize in the calling environment.
#' @name try_memoize
try_memoize <- function(fn) {
  if ('memoise' %in% installed.packages()) {
    require(memoise)
    eval.parent(substitute(memoise(fn)))
  }
}

# A reference class that implements a stack data structure.
stack <- setRefClass('stack', list(elements = 'list'), methods = list(
  initialize = function()  { elements <<- list() },
  empty      = function()  { length(elements) == 0 },
  push       = function(x) { elements[[length(elements) + 1]] <<- x },
  pop        = function()  {
    if (length(elements) == 0) stop("syberiaStructure:::stack is empty")
    tmp <- tail(elements, 1)[[1]]
    elements[[length(elements)]] <<- NULL
    tmp
  },
  pop_all    = function()  { tmp <- elements; elements <<- list(); tmp }
))                                                                      

#' Whether or not a directory is an idempotent resource.
#'
#' By definition, this means the directory contains a file with the same name
#' (ignoring extension) as the directory.
#'
#' @param dir character. The directory to check.
#' @return \code{TRUE} or \code{FALSE} according as the directory is idempotent.
#'   There is no checking to ensure the directory exists.
#' @examples
#' \dontrun{
#'   # If we have a directory foo containing foo.R, then
#'   is.idempotent_directory('foo')
#'   # is TRUE, otherwise it's FALSE.
#' }
is.idempotent_directory <- function(dir) {
  # TODO: (RK) Case insensitivity in OSes that don't respect it, i.e. Windows?
  # TODO: (RK) File extensions besides .r and .R?
  extensionless_exists(file.path(dir, basename(dir)))
}

#' Determine whether an R file exists regardless of case of extension.
#'
#' @param filename character. The filename to test (possibly without extension).
#' @return \code{TRUE} or \code{FALSE} if the filename exists regardless of 
#'   R extension.
#' @examples
#' \dontrun{
#'  # Assume we have a file \code{"foo.R"}. The following all return \code{TRUE}.
#'  extensionless_exists('foo.R')
#'  extensionless_exists('foo.r')
#'  extensionless_exists('foo')
#' }
extensionless_exists <- function(filename) {
  file.exists(paste0(strip_r_extension(filename), '.r')) ||
  file.exists(paste0(strip_r_extension(filename), '.R')) 
  # Don't use the any + sapply trick because we can skip the latter check if the
  # former succeeds.
}

#' Strip R extension.
#'
#' @param filename character. The filename to strip.
#' @return the filename without the '.r' or '.R' at the end.
strip_r_extension <- function(filename) {
  stopifnot(is.character(filename))
  gsub("\\.[rR]$", "", filename)
}

#' Strip a root file path from an absolute filename.
#'
#' @param root character. The root path.
#' @param filename character. The full file name.
#' @return the stripped file path.
#' @examples
#' \dontrun{
#'   stopifnot("test" == strip_root("foo/bar/test", "test"))
#' }
strip_root <- function(root, filename) {
  stopifnot(is.character(root) && is.character(filename))
  if (substring(filename, 1, nchar(root)) == root) {
    filename <- substring(filename, nchar(root) + 1, nchar(filename)) 
    gsub("^\\/", "", filename)
  } else filename
}

#' Convert an idempotent resource name to a non-idempotent resource name.
#'
#' @param filename character. The filename to convert.
#' @return the non-idempotent filename.
drop_idempotence <- function(filename) {
  if (basename(dirname(filename)) == basename(filename))
    dirname(filename)
  else filename
}

#' Convert a filename to a resource name.
#'
#' @param filename character. The filename.
#' @return the resource name (i.e., stripped of idempotence and extension).
resource_name <- function(filename) {
  drop_idempotence(strip_r_extension(filename))
}

