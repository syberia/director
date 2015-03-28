#' Define a search pattern for use with the find method on a director.
#'
#' A search pattern is one of the following:
#'
#'   \describe{
#'     \item{exact}{ match. The strings must match exactly this value.}
#'     \item{partial}{ match. The strings which contain this string as
#'        a substring will be matched.}
#'     \item{wildcard}{ match. Fuzzy matching like in the ctrl+p plugin
#'        for vim. If the pattern is "abc", it will be translated to the
#'        regular expression ".*a.*b.*c.*", that is, any characters followed
#'        by an 'a' followed by any characters followed by a 'b' followed by
#'        any characters followed by a 'c' followed by any characters (e.g.,
#'        "fabulous cake").}
#'      \item{regex}{ match. Apply a regular expression filter to the
#'        set of strings.}
#'   }
#' 
#' @param pattern character. The pattern to search for.
#' @param method character. The search pattern method, one of "exact",
#'    "partial", or "wildcard".
#' @note Patterns can be combined using the \code{|} and \code{&} operators.
#' @examples
#' \dontrun{
#'   d$find(search_pattern("this/file", "exact"))
#'   # If d is a director object, the above will find exactly the resource
#'   # "this/file".
#' 
#'   d$find(search_pattern("this", "partial")
#'   # The above will find any resource containing "this" as a substring.
#'
#'   d$find(search_pattern("this", "wildcard")
#'   # The above will find any resource containing the consecutive letters
#'   # "this" separated by arbitrary strings.
#'
#'   d$find(search_pattern("foobar", "partial") | search_pattern("root", "exact"))
#'   # The above will find any resource with the substring "foobar" or having
#'   # exactly the name "root".
#' }
search_pattern <- function(pattern, method) {
  msg <- function(x) {
    stop("Search ", deparse(substitute(x)) ," must be of type character; ",
         "instead I got a ", class(x)[1])
  }

  if (!is.character(method)) { msg(method) }
  if (!is.character(pattern)) { msg(pattern) }

  search_pattern_(pattern, tolower(method))
}

search_pattern_ <- function(pattern, method) {
  if (is.search_pattern_join(pattern)) { pattern }
  else if (is.search_pattern_join(method)) { method }
  else if (length(pattern) > 1) {
    Reduce(function(x, y) {
      search_pattern_(x, method) | search_pattern_(y, method)
    }, pattern)
  } else if (length(method) > 1) {
    Reduce(function(x, y) {
      search_pattern_(pattern, x) | search_pattern_(pattern, y)
    }, method)
  } else {
    `verify_search_pattern_method!`(method)
    as.search_pattern(list(pattern = pattern, method = method))
  }
}

`verify_search_pattern_method!` <- function(method) {
  ok <- exists(paste0("apply_pattern.", method), envir = getNamespace("director"))
  if (!ok) { stop("Invalid search pattern.") }
}

search_pattern_join <- function(pattern1, pattern2, type) {
  stopifnot(identical(type, "and") || identical(type, "or"))
  as.search_pattern(structure(list(pattern1, pattern2, type = type),
                              class = c("search_pattern_join")))
}

as.search_pattern <- function(x) {
  class(x) <- c(class(x), "search_pattern")
  x
}

is.search_pattern <- function(x) { is(x, "search_pattern") }
is.atomic_search_pattern <- function(x) {
  is.search_pattern(x) && !is.search_pattern_join(x)
}
is.search_pattern_join <- function(x) { is(x, "search_pattern_join") }

`|.search_pattern` <- function(e1, e2) {
  stopifnot(is(e2, "search_pattern"))

  search_pattern_join(e1, e2, type = "or")
}

`&.search_pattern` <- function(e1, e2) {
  stopifnot(is(e2, "search_pattern"))

  search_pattern_join(e1, e2, type = "and")
}

#' Apply a pattern filter to a character vector.
#' 
#' @param pattern search_pattern.
#' @param strings character. The strings to filter down.
apply_pattern <- function(pattern, strings) {
  if (is.atomic_search_pattern(pattern)) {
    class(pattern) <- c(pattern$method, class(pattern))
    UseMethod("apply_pattern", object = pattern)
  } else if (is.search_pattern_join(pattern)) {
    operand <- if (pattern$type == "and") { intersect } else { union }
    operand(Recall(pattern[[1]], strings), Recall(pattern[[2]], strings))
  } else { stop("Invalid pattern") }
}

apply_pattern.exact <- function(pattern, strings) {
  if (any(pattern$pattern == strings)) { pattern$pattern }
  else { character(0) }
}

apply_pattern.wildcard <- function(pattern, strings) {
  pattern <- gsub("([]./\\*+()])", "\\\\\\1", pattern$pattern)
  pattern <- gsub("([^\\$^])", ".*\\1", pattern) # turn this into ctrl+p
  pattern <- gsub("^.*", "^", pattern, fixed = TRUE)
  grep(pattern, strings, value = TRUE)
}

apply_pattern.partial <- function(pattern, strings) {
  grep(pattern$pattern, strings, fixed = TRUE, value = TRUE)
}

apply_pattern.regex <- function(pattern, strings) {
  grep(pattern$pattern, strings, value = TRUE)
}

apply_pattern.idempotence <- function(pattern, strings) {
  # TODO: (RK) Consider the string "."
  idempotent <- vapply(strings, function(x) basename(x) == basename(dirname(x)), logical(1))
  idem_dirs  <- dirname(strings[idempotent])
  helpers <- vapply(strings, function(x) dirname(x) %in% idem_dirs, logical(1))
  strings[idempotent] <- idem_dirs
  strings <- strings[!helpers | idempotent]
  return(strings)
}

