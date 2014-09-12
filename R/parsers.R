#' Register a resource parser.
#'
#' @param path character. The prefix to look for in the director.
#' @param parser function. 
#' @param overwrite logical. If \code{TRUE}, \code{register_parser} will overwrite
#'   the route instead of erroring if the path already has a registered
#'   parser. The default is \code{FALSE}.
#' @param cache logical. Whether or not to cache resources processed with this
#'   parser. Cached resources will not be re-parsed if their dependencies have
#'   not been modified. This distinction is important, as most resources are
#'   factory resources (the object they generate should not be shared across
#'   the entire project; instead, a copy should be made). The default is \code{FALSE}.
#' @examples
#' \dontrun{
#'   d <- director('some/project')
#'   d$register_parser('models', function() { print("I am a ", resource, ", a model!") })
#'   r <- d$resource('models/some_model.R')
#'   r$value() # Will print: I am a models/some_model, a model!
#' }
register_parser <- function(path, parser, overwrite = FALSE, cache = FALSE) {
  stopifnot(is.character(path))
  parser <- if (missing(parser)) function() { } else parser
  stopifnot(is.function(parser))
  if (length(formals(parser)) != 0) {
    formals(parser) <- NULL # TODO: (RK) Record required uses?
  }
  
  if (is.element(path, names(.parsers)) && !isTRUE(overwrite)) {
    stop("Parser already registered for path ", sQuote(path))
  }

  if (isTRUE(cache)) .cached_resources <<- c(.cached_resources, path)

  .parsers[[path]] <<- parser
  .parsers <<- .parsers[names(.parsers)[rev(order(sapply(names(.parsers), nchar)))]]
}

