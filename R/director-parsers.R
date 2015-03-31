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
register_parser <- function(path, parser = function() { }, overwrite = FALSE, cache = FALSE) {
  enforce_type(path,   "character", "director$register_parser")
  enforce_type(parser, "function",  "director$register_parser")

  if (length(path) != 1) {
    stop("A parser must be registered to a path that is a scalar character ",
         "but instead I got a character vector of length",
          crayon::red(as.character(length(path))), ".")
  }

  if (length(formals(parser)) != 0) {
    # TODO: (RK) Require correct formals specification: https://github.com/robertzk/director/issues/21
    formals(parser) <- NULL
  }
  
  if (is.element(paste0("/", path), names(.parsers)) && !isTRUE(overwrite)) {
    stop("Parser already registered for path ", crayon::red(path))
  }

  if (isTRUE(cache)) {
    .cached_resources <<- c(.cached_resources, path)
  }

  # Prefix "/" for empty paths.
  .parsers[[paste0("/", path)]] <<- parser

  ## We store each parser function by path in descending order by length.
  ## This will favor paths that are more fully specified. For example,
  ## if we have a parser for `"models"` and a parser  for `"models/ensembles"`,
  ## the latter has a longer length and will be preferred when selecting the 
  ## function used for parsing resources in the `"models/ensembles"` directory.
  .parsers         <<- .parsers[names(.parsers)[rev(order(sapply(names(.parsers), nchar)))]]
}

