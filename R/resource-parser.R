#' Apply the parser to a resource.
#'
#' @name parser
#' @aliases parsers
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
#' @param parse. logical. Whether or not to apply the \link{parser} to the
#'    resource. If \code{FALSE}, this is equivalent to sourcing the resource's
#'    file without running its parser. By default, \code{TRUE}.
#' @return The parsed resource, usually some useful R object.
#' @note The parameters must be named \code{object} and \code{...} due to
#'    this method's inclusion in a \code{\link{tower}}.
parser <- function(object, ...) {
  director <- object$resource$director

  route <- director$match_parser(object$resource$name)

  if (is.null(route)) {
    # No parser for this resource.
    # Use the default parser, just grab the value.
    object <- object$preprocessed$value
  } else {
    fn <- director$parser(route)
    environment(fn) <- new.env(parent = environment(fn)) %<<% list(
      # TODO: (RK) Intersect with parser formals.
      # TODO: (RK) Use alist so these aren't evaluated right away.
       resource = object$resource$name,
       input = object$state$preprocessor.source_env,
       output = object$preprocessed$value,
       director = director,
       preprocessor_output = object$preprocessed$preprocessor_output,
       filename = object$state$filename,
       args = list(...),
       "%||%" = function(x, y) if (is.null(x)) y else x
    )
    object <- fn()
  }

  yield()
}

