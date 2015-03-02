search_pattern <- function(pattern, method) {
  if (!(is.character(method) && length(method) == 1) && 
        is.element(tolower(method), c("wildcard", "partial", "exact"))) {
    stop("Search method must be one of 'wildcard', 'partial', or ",
         "'exact'.")
  }
  method <- tolower(method)

  if (!is.character(pattern)) {
    stop("Search pattern must be of type character; instead I got a ",
         class(pattern)[1])
  }

  if (length(pattern) > 1) {
    Reduce(function(x, y) {
      search_pattern(x, method) | search_pattern(y, method)
    }, pattern)
  } else {
    structure(list(pattern = pattern, method = method), class = "search_pattern")
  }
}
