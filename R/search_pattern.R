search_pattern <- function(pattern, method) {
  msg <- function(x) {
    stop("Search ", deparse(substitute(x)) ," must be of type character; ",
         "instead I got a ", class(x)[1])
  }

  if (!is.character(method)) { msg(method) }
  if (!is.character(pattern)) { msg(pattern) }
  method <- tolower(method)

  search_pattern_(pattern, method)
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
  if (!is.element(tolower(method), c("wildcard", "partial", "exact"))) {
    stop("Search method must be one of 'wildcard', 'partial', or ",
         "'exact'.")
  }
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
  }
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
  grep(pattern, strings, fixed = TRUE, value = TRUE)
}


