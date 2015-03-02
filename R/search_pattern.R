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
    Reduce(operand, lapply(pattern[1:2], apply_pattern, strings))
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

apply_pattern.idempotence <- function(pattern, strings) {
  safe_get <- function(x, i) { if (i <= 0) { i } else { x[[i]] } }
  sep <- .Platform$file.sep
  idempotent <-
    Filter(function(x) { identical(safe_get(x, length(x)), safe_get(x, length(x) - 1)) },
           strsplit(strings, sep))
  idempotent_prefixes <- vapply(lapply(idempotent, utils::head, -1),
    paste, character(1), collapse = sep)
  is_atomic_prefix_of <- function(x, y) {
    vapply(y, function(z) {
      any(vapply(x, function(w) {
        identical(substring(z, 1, nchar(w)), w) &&
        !grepl(sep, substring(z, nchar(w) + 1, nchar(z)), fixed = TRUE)
      }, logical(1)))
    }, logical(1))
  }
  idempotent <- vapply(idempotent, paste, character(1), collapse = sep)
  non_idempotent <- strings[is_atomic_prefix_of(paste0(idempotent_prefixes, sep), strings)]
  non_idempotent <- setdiff(non_idempotent, idempotent)
  c(idempotent_prefixes, setdiff(strings, c(idempotent, non_idempotent)))
}

