#' Compile a resource using a resource handler.
#'
#' @param resource_object directorResource. The resource object to compile.
#' @param source_args list. The arguments to the \code{base::source} call.
#'    Must include \code{local} of type \code{environment}.
#' @param resource_name character. The name of the resource. This
#'    is used to fetch the resource handler.
#' @param tracking logical. Whether or not to perform modification tracking
#'   by pushing accessed resources to the director's stack. The default is
#'   \code{TRUE}.
#' @return A list with keys \code{value, modified} containing the compiled
#'   value (some R object) and a \code{logical} flag that indicates whether
#'   or not any of the resources used have been modified.
compile <- function(resource_object, source_args, resource_name, tracking = TRUE) {
#  if (!is.element('local', names(source_args)))
#    stop("To compile ", sQuote(source_args[[1]]), " you must include ",
#         dQuote('local'), " in the list of arguments to pass to base::source")
#  else if (!is.environment(source_args$local))
#    stop("To compile ", sQuote(source_args[[1]]), " you must include an ",
#         "environment in the ", dQuote('local'), " parameter to base::source.")
#
#  .track <<- TRUE
#  .stack$clear()
#  on.exit(.track <<- FALSE)
#   
#  source_args$local$resource <- function(...) .self$resource(...)
#
#  value <- do.call(base::source, source_args)$value
#  modified <- any(vapply(.stack$pop_all(), getElement, logical(1), name = 'modified'))
#
#  list(value = value, modified = modified)
#  value
}

