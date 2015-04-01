#' Apply the preprocessor to a resource.
#'
#' Hand in hand with parsers, preprocessors are the heart of director. The idea
#' is to allow the developer to do any additional stuff prior to sourcing an R
#' file. For example, if some helper functions are desired or some operators
#' should be overloaded for a DSL (domain-specific language), the preprocessor
#' can perform this prior to sourcing the R file.
#' 
#' To define a preprocessor for routes in a given path, you can use the
#' \code{\link{register_preprocessor}} method on the director object.
#'
#' @name preprocessor
#' @aliases preprocessors
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
#' @param parse. logical. Whether or not to apply the \link{parser} to the
#'    resource. If \code{FALSE}, this is equivalent to sourcing the resource's
#'    file without running its parser. By default, \code{TRUE}.
#' @seealso \code{\link{register_preprocessor}}, \code{\link{active_resource}},
#'    \code{\link{tower}}
#' @return The parsed resource if \code{parse. = TRUE}, and the preprocessed
#'    resource otherwise.
#' @note The parameters must be named \code{object} and \code{...} due to
#'    this method's inclusion in a \code{\link{tower}}.
preprocessor <- function(object, ..., parse. = TRUE) {
  director <- object$resource$director

  route <- director$match_preprocessor(object$resource$name)

  if (isTRUE(object$injects$virtual)) {
    filename <- NULL
  } else {
    filename <- object$state$filename <-
      director$filename(object$resource$name, absolute = TRUE)
  }

  object$injects %<<% list(
    # TODO: (RK) Use alist so these aren't evaluated right away.
    root = director$root,
    # TODO: (RK) Use find_director helper to go off root + project_name
    resource = function(...) director$resource(...),
    resource_name = object$resource$name,
    resource_exists = function(...) director$exists(...),
    helper = NULL # TODO: (RK) Allow helper parsing.
  )

  if (is.null(route)) {
    # No preprocessor for this resource.
    # Use the default preprocessor, base::source.
    default_preprocessor <- function(filename) {
      # TODO: (RK) Figure out correct environment assignment.
      base::source(filename, local = source_env)$value
    }
    source_env <- new.env(parent = parent.env(topenv(parent.env(environment())))) %<<%
      object$injects
    object$state$preprocessor.source_env <- source_env
    environment(default_preprocessor) <- 
      new.env(parent = object$resource$defining_environment) %<<%
      list(source_env = source_env)

    object$preprocessed <- list(
      value = default_preprocessor(filename),
      preprocessor_output = new.env(parent = emptyenv())
    )
  } else {

    object$state$preprocessor.source_env <- new.env(parent = object$injects)

    preprocessor_output <- new.env(parent = emptyenv())
    fn <- director$preprocessor(route)
    environment(fn) <- new.env(parent = environment(fn)) %<<% object$injects %<<% list(
      # TODO: (RK) Intersect with preprocessor formals.
      # TODO: (RK) Use alist so these aren't evaluated right away.
       resource = object$resource$name,
       director = director,
       filename = filename,
       args = list(...),
       source_env = object$state$preprocessor.source_env,
       source = function() eval.parent(quote(base::source(filename, source_env)$value)),
       preprocessor_output = preprocessor_output,
       "%||%" = function(x, y) if (is.null(x)) y else x
    )

    object$preprocessed <- list(
      value = fn(),
      preprocessor_output = preprocessor_output
    )
  }

  if (isTRUE(parse.)) {
    yield() # Apply parser.
  } else {
    object$preprocessed$value
  }
}

