`%||%` <- function(x, y) if (is.null(x)) y else x


# Dynamically create an accessor method for reference classes.
accessor_method <- function(attr) {
  fn <- eval(bquote(
    function(`*VALUE*` = NULL)
      if (missing(`*VALUE*`)) .(substitute(attr))
      else .(substitute(attr)) <<- `*VALUE*`
  ))
  environment(fn) <- parent.frame()
  fn
}

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
  fn
}

# A reference class that implements a stack data structure.
shtack <- setRefClass('stack', list(elements = 'list'), methods = list(
  clear      = function()  { elements <<- list() },
  empty      = function()  { length(elements) == 0 },
  push       = function(x) { elements[[length(elements) + 1]] <<- x },
  peek       = function(n = 1)  {
    if (isTRUE(n)) return(elements)
    els <- seq(length(elements), length(elements) - n + 1)
    if (length(els) == 1) elements[[els]]
    else elements[els]
  },
  pop        = function()  {
    if (length(elements) == 0) stop("director:::stack is empty")
    tmp <- elements[[length(elements)]]
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
    gsub("^\\/*", "", filename)
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

#' Create a resource cache key from a resource key.
#'
#' This is the key under whose director cache the info about the resource
#' as of previous execution will be stored.
#'
#' @param resource_key character. The resource key.
#' @return a cache key, currently just \code{"resource_cache/"} followed by
#'    the \code{resource_key}.
resource_cache_key <- function(resource_key) {
 file.path('resource_cache', resource_key)
}

#' Get all helper files associated with an idempotent resource directory.
#'
#' @param path character. The *absolute* path of the idempotent resource.
#' @param ... additional parameters to pass to \code{list.files}.
#' @param leave_idempotent logical. Whether or not to leave the
#'   idempotent file (non-helper). By default \code{FALSE}.
#' @return a character list of relative helper paths.
#' @examples
#' \dontrun{
#'   # If we have a directory structure given by \code{"model/model.R"},
#'   # \code{"model/constants.R"}, \code{"model/functions.R"}, then the
#'   # below will return \code{c("constants.R", "functions.R")}.
#'   get_helpers("model")
#' }
get_helpers <- function(path, ..., leave_idempotent = FALSE) {
  helper_files <- list.files(path, pattern = '\\.[rR]$', ...)
  if (leave_idempotent) {
    helper_files
  } else {
    same_file <- which(vapply(helper_files, 
      function(f) basename(strip_r_extension(f)) == basename(path), logical(1)))
    helper_files[-same_file]
  }
}

#' Whether or not any substring of a string is any of a set of strings.
#'
#' @param string character.
#' @param set_of_strings character.
#' @return logical
#' @examples
#' stopifnot(director:::any_is_substring_of('test', c('blah', 'te', 'woo'))) # TRUE
#' stopifnot(!director:::any_is_substring_of('test', c('blah', 'woo'))) # FALSE
any_is_substring_of <- function(string, set_of_strings) {
  any(vapply(set_of_strings,
             function(x) substring(string, 1, nchar(x)) == x, logical(1)))
}

# Stolen from testthat:::colourise
.fg_colours <- 
  structure(c("0;30", "0;34", "0;32", "0;36", "0;31", "0;35", "0;33",
  "0;37", "1;30", "1;34", "1;32", "1;36", "1;31", "1;35", "1;33",
  "1;37"), .Names = c("black", "blue", "green", "cyan", "red",
  "purple", "brown", "light gray", "dark gray", "light blue", "light green",
  "light cyan", "light red", "light purple", "yellow", "white"))
.bg_colours <- 
  structure(c("40", "41", "42", "43", "44", "45", "46", "47"), .Names = c("black",
  "red", "green", "brown", "blue", "purple", "cyan", "light gray"
  ))

colourise <- function (text, fg = "black", bg = NULL) {
  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color", "xterm-256color", "screen",
      "screen-256color")
  if (!any(term %in% colour_terms, na.rm = TRUE)) return(text)
  col_escape <- function(col) paste0("\033[", col, "m")
  col <- .fg_colours[tolower(fg)]
  if (!is.null(bg)) col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}



