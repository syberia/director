#' Register a resource preprocessor
#'
#' @param path character. The prefix to look for in the director.
#' @param preprocessor function. 
#' @param overwrite logical. If \code{TRUE}, \code{register_preprocessor} will overwrite
#'   the route instead of erroring if the path already has a registered
#'   preprocessor. The default is \code{FALSE}.
#' @examples
#' \dontrun{
#'   d <- director("some/project")
#'   d$register_preprocessor('models', function() { print("I am a ", resource, ", a model!") })
#'   r <- d$resource("models/some_model.R")
#'   r$value() # Will print: I am a models/some_model, a model!
#' }
register_preprocessor <- function(path, preprocessor, overwrite = FALSE) {
  enforce_type(path,         "character", "director$register_preprocessor")
  enforce_type(preprocessor, "function",  "director$register_preprocessor")

  if (length(formals(preprocessor)) != 0) {
    # TODO: (RK) Require correct formals specification: https://github.com/robertzk/director/issues/21
    formals(preprocessor) <- NULL
  }
  
  if (is.element(path, paste0("/", names(.preprocessors))) && !isTRUE(overwrite)) {
    stop("Preprocessor already registered for path ", crayon::red(path), ".")
  }

  # Prefix "/" for empty paths.
  .preprocessors[[paste0("/", path)]] <<- preprocessor

  ## We store each preprocessor function by path in descending order by length.
  ## This will favor paths that are more fully specified. For example,
  ## if we have a preprocessor for `"models"` and a preprocessor  for
  ## `"models/ensembles"`, ## the latter has a longer length and will be
  ## preferred when selecting the function used for parsing resources in
  ## the `"models/ensembles"` directory.
  .preprocessors         <<- .preprocessors[
    names(.preprocessors)[rev(order(sapply(names(.preprocessors), nchar)))]]
}

#' Whether there exists a preprocessor for a resource.
#'
#' @param resource_path character. The resource name.
#' @return \code{TRUE} or \code{FALSE} depending on whether there
#'   is a preprocessor for this resource.
has_preprocessor <- function(resource_path) {
  !is.null(self$match_preprocessor(resource_path))
}

