#' Compile a resource using a resource handler.
#'
#' @param value ANY. The \code{value} obtained from sourcing the resource
#'    file.
#' @param provides environment. The environment provided when sourcing
#'    the resource.
#' @param resource_name character. The name of the resource. This
#'    is used to fetch the resource handler.
#' @param tracking logical. Whether or not to perform modification tracking
#'   by pushing accessed resources to the director's stack.
#' @return the compiled resource.
compile <- function(value, provides, resource_name) {
  # TODO: (RK) Compile the resource here.
  value
}

