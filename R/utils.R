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
#' @param fn function. The function to memoize.
#' @return nothing, but \code{try_memoize} will use non-standard
#'   evaluation to memoize in the calling environment.
#' @name try_memoize
try_memoize <- function(fn) {
  if (requireNamespace("memoise", quietly = TRUE)) {
    eval.parent(substitute(memoise::memoise(fn)))
  } else {
    fn
  }
}

# A reference class that implements a stack data structure.
shtack <- methods::setRefClass('stack', list(elements = 'list'), methods = list(
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

#' Complete the extension of a file (.r or .R).
#'
#' @note This function assumes at least one file ending in .r or .R exists.
#' @param name character. The filename sans extension.
#' @param base character. A base path to be prefixed to \code{name} when
#'   checking if the suffixed versions exist. The final returned string will
#'   not include this base.
#' @return \code{name} suffixed by ".r" or ".R" according to which exists.
#'   (Many Unix-based systems are extension case-sensitive).
#' @examples
#' \dontrun{
#'  # Assume we have a file \code{"foo.R"}.
#'  stopifnot(complete_extension("foo") == "foo.R")
#'
#'  # Assume we have a file \code{"bar.r"}.
#'  stopifnot(complete_extension("bar") == "bar.R")
#' }
complete_extension <- function(name, base = NULL) {
  upper_r <- paste0(name, ".R")
  filepath <- if (missing(base)) upper_r else file.path(base, upper_r)
  if (file.exists(filepath)) {
   upper_r 
  } else {
    paste0(name, ".r")
  }
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
#' @return a character list of relative helper paths.
#' @examples
#' \dontrun{
#'   # If we have a directory structure given by \code{"model/model.R"},
#'   # \code{"model/constants.R"}, \code{"model/functions.R"}, then the
#'   # below will return \code{c("constants.R", "functions.R")}.
#'   get_helpers("model")
#' }
get_helpers <- function(path) {
  helper_files <- list.files(path, pattern = '\\.[rR]$') # TODO: (RK) Recursive helpers?
  same_file <- which(vapply(helper_files, 
    function(f) strip_r_extension(f) == basename(path), logical(1)))
  helper_files <- helper_files[-same_file]
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

#' Enforce parameter types (logical, character, etc.).
#'
#' @param object ANY. An R object to enforce types on.
#' @param admissible_types character. A character vector of allowed types. 
#' @param function_name character. The function this enforcement is occurring
#'    in, for error messages.
#' @param name character. The name of the parameter whose type is being
#'    enforced. By default, the string expression passed in to the first
#'    argument, \code{object}.
#' @return Nothing, but error if the type does not match.
#' @examples
#' \dontrun{
#' x <- 1
#' enforce_type(x, "logical", "myfunction")
#' # Will call stop() with the following error:
#' # "In 'myfunction', the 'x' parameter must be a character; instead, I got
#' # a logical.
#' }
enforce_type <- function(object, admissible_types, function_name, name = deparse(substitute(object))) {
  ## The `is` function takes parameters `object` and `class`, so this
  ## sneaky call is equivalent to 
  ## 
  ## ```r
  ## any(sapply(admissible_types, function(type) is(object, type)))
  ## ```
  if (!any(vapply(admissible_types, is, logical(1), object = object))) {
    stop(call. = FALSE, "In ", crayon::blue(function_name), ", the ",
         crayon::blue(name), " parameter must be a ",
         crayon::green(paste(admissible_types, collapse = " or ")),
         "; instead, I got a ", crayon::red(class(object)[1]), ".")
  }
}

#' A simple caching structure.
#'
#' @return A list of four methods \code{get}, \code{set}, \code{exists}
#'   and \code{unset} that modify another list under the hood.
simple_cache <- function() {
  cache <- list()
  list(
    get    = function(key) cache[[key]],
    ## If instead we said `cache[[key]] <<- value`, `NULL` values would be
    ## cached incorrectly, since assigning `NULL` remove the key.
    set    = function(key, value) cache[key] <<- structure(list(value), .Names = key),
    exists = function(key) is.element(key, names(key)),
    unset  = function(key) cache[[key]] <<- NULL
  )
}

#' Duplicate a function object.
#'
#' @param original function.
#' @useDynLib director duplicate_
duplicate <- function(original) {
  .Call(duplicate_, original)
}

#' Append to a list or environment, overwriting if necessary.
#'
#' @param obj1. The object to be appended to.
#' @param obj2. The object to append.
#' @examples
#' \dontrun{
#'   x <- list(a = 1)
#'   x %<<% list(b = 2) # list(a = 1, b = 2)
#'   x %<<% list(a = 2) # list(a = 2)
#'   y <- list2env(x)
#'   y %<<% list(b = 2) # environment with a = 1 and b = 2
#'   y %<<% list2env(list(b = 2)) # same as above
#'   y %<<% list(a = 2) # environment with a = 2
#' }
`%<<%` <- function(obj1, obj2) {
  all_named <- function(x) { !is.null(names(x)) && all(nzchar(names(x))) }
  if (is.list(obj1)) stopifnot(all_named(obj1))
  if (is.list(obj2)) stopifnot(all_named(obj2))

  for (name in ls(obj2)) {
    obj1[[name]] <- obj2[[name]]
  }
  obj1
}

#' Queue with size limit.
#'
#' If you push more elements onto the queue than it has room for, they will
#' fall off screaming and wailing.
#'
#' @param size integer. Maximum number of elements in the queue.
#' @examples
#' \dontrun{
#'   q <- sized_queue(size = 2)
#'   q$push(1)
#'   q$get(1) # 1
#'   q$get(2) # NULL
#'   q$push(2)
#'   q$get(1) # 2
#'   q$get(2) # 1
#'   q$push(3)
#'   q$get(1) # 3
#'   q$get(2) # 2
#'   q$get(3) # NULL
#' }
sized_queue <- function(size) {
  queue <- vector('list', size)
  structure(class = "sized_queue", list(
    push = function(el) {
      queue <<- append(list(el), queue)[seq_len(size)]
    },
    get = function(el) queue[[el]]
  ))
}
