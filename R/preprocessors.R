#' Register a resource preprocessor
#'
#' @param path character. The prefix to look for in the director.
#' @param preprocessor function. 
#' @param overwrite logical. If \code{TRUE}, \code{register_preprocessor} will overwrite
#'   the route instead of erroring if the path already has a registered
#'   preprocessor. The default is \code{FALSE}.
#' @examples
#' \dontrun{
#'   d <- director('some/project')
#'   d$register_preprocessor('models', function() { print("I am a ", resource, ", a model!") })
#'   r <- d$resource('models/some_model.R')
#'   r$value() # Will print: I am a models/some_model, a model!
#' }
register_preprocessor <- function(path, preprocessor, overwrite = FALSE) {
  stopifnot(is.character(path))
  stopifnot(is.function(preprocessor))
  if (length(formals(preprocessor)) != 0) {
    formals(preprocessor) <- NULL # TODO: (RK) Record required uses?
  }
  
  if (is.element(path, names(.preprocessors)) && !isTRUE(overwrite)) {
    stop("Preprocessor already registered for path ", sQuote(path))
  }

  .preprocessors[[path]] <<- preprocessor
  .preprocessors <<- .preprocessors[
    names(.preprocessors)[rev(order(sapply(names(.preprocessors), nchar)))]]
}

#' Whether there exists a preprocessor for a resource.
#'
#' @param resource_path character. The resource name.
has_preprocessor <- function(resource_path) {
  !is.null(Find(function(x) substring(resource_path, 1, nchar(x)) == x,
    names(.self$.preprocessors)))
}

