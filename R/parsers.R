#' Register a resource parser.
#'
#' @param path character. The prefix to look for in the director.
#' @param parser directorParser. 
#' @param overwrite logical. If \code{TRUE}, \code{register_parser} will overwrite
#'   the route instead of erroring if the path already has a registered
#'   parser. The default is \code{FALSE}.
#' @examples
#' \dontrun{
#'   d <- director('some/project')
#'   d$register_parser('models', function() { print("I am a ", resource, ", a model!") })
#'   r <- d$resource('models/some_model.R')
#'   r$value() # Will print: I am a models/some_model, a model!
#' }
register_parser <- function(path, parser, overwrite = FALSE) {
  stopifnot(is.character(path))
  stopifnot(is.function(parser))
  stopifnot(length(formals(parser)) == 0) # Use environment to provide info
  
  if (is.element(path, names(.parsers)) && !isTRUE(overwrite)) {
    stop("Parser already registered for path ", sQuote(path))
  }

  .parsers[[path]] <<- parser
  .parsers <<- .parsers[names(.parsers)[rev(order(sapply(names(.parsers), nchar)))]]
}

