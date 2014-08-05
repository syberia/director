#' Register a resource parser.
#'
#' @param path character. The prefix to look for in the director.
#' @param parser directorParser. 
register_parser <- function(path, parser) {
  stopifnot(is.character(path))
  stopifnot(is.function(parser))
  stopifnot(length(formals(parser)) == 3 ||
            is.element('...', names(formals(parser))))
  
  if (is.element(path, names(.parsers))) {
    stop("Parser already registered for path ", sQuote(path))
  }

  .parsers[[path]] <<- parser
  .parsers <<- .parsers[names(.parsers)[rev(order(sapply(names(.parsers), nchar)))]]
}

