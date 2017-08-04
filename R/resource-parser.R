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
#' @param parser.route character. If given, use an explicit parser route
#'    instead of inferring it from the filename.
#' @return The parsed resource, usually some useful R object.
#' @note The parameters must be named \code{object} and \code{...} due to
#'    this method's inclusion in a \code{\link{tower}}.
parser <- function(object, ..., parser.route = NULL) {
  director <- object$resource$director

  route <- parser.route %||% director$match_parser(object$resource$name)

  if (is.null(route)) {
    # No parser for this resource.
    # Use the default parser, just grab the value.
    object <- object$resource$preprocessed$value
  } else {
    args <- list(...)
    # TODO: (RK) Use `args` to carry around to prevent partial matching
    # on "object" ...
    object <- apply_parser(object, route, args)
  }

  ## This will yield to the identity function, simply returning `object`
  ## and thus the parsed resource. Every function in a `tower` (see tower.R)
  ## must call `yield`. This also encourages us to plan ahead if we ever
  ## want to add even further layers in the resource parsing tower.
  yield()
}

apply_parser <- function(active_resource, route, args) {
  director <- active_resource$resource$director

  parser_function <- director$parser(route)

  sourcing_env <-
    new.env(parent = director_state$defining_environment) %<<%
    environment(parser_function)

  environment(parser_function) <-
    new.env(parent = sourcing_env) %<<%
           active_resource$injects %<<% list(
    # TODO: (RK) Intersect with parser formals.
    # TODO: (RK) Use alist so these aren't evaluated right away.
     resource = active_resource$resource$name,
     input    = active_resource$state$preprocessor.source_env,
     output   = output_for_resource(active_resource$resource),
     director = director,
     preprocessor_output = active_resource$resource$preprocessed$preprocessor_output,
     filename = active_resource$injects$filename,
     args = args,
     "%||%" = function(x, y) if (is.null(x)) y else x
  )
  parser_function()
}

output_for_resource <- function(resource) {
  UseMethod("output_for_resource")
}

output_for_resource.director_resource <- function(resource) { 
  resource$preprocessed$value
}


