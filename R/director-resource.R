#' Fetch a resource relative to a director object.
#'
#' Resources are R scripts that optionally have a "parser" attached
#' which takes the result of executing the file, including all of its local
#' variables, and does some additional computation. This is useful if,
#' for example, you are trying to define a standard format for creating a
#' reference class object by specifying some inputs, but want to make it
#' easy to provide those inputs by users.
#' 
#' This method will return a \code{directorResource} object that represents
#' that particular R script. A resource can have a \code{\link[=register_preprocessor]{preprocessor}}
#' and a \code{link[=register_parser]{parser}} attached to it.
#'
#' The former determines how to source the R file. For example, if you need
#' to inject additional variables prior to sourcing it, you can do so
#' from the preprocessor.
#' 
#' The parser determines what to do with the R file after sourcing it.
#' It can tell what the dependencies of the file are (i.e., what other
#' resources were used when sourcing it), and whether or not it was modified
#' (i.e., whether the R file or any of its dependencies were modified).
#' 
#' Together, a preprocessor, parser, and source file compose a resource.
#'
#' @param name character. The name of the resource (i.e. R script) relative
#'   to the root of the director object.
#' @param provides list or environment. A list or environment of values to provide
#'   to the resource. The default is nothing, i.e., \code{list()}. Note that
#'   \code{provides} will be coerced to an environment, and its parent 
#'   environment will be set to \code{parent.env(topenv())} to prevent
#'   access to global variables (and encourage modularity and lack of side
#'   effects. There should always be a way to write your code without them).
#' @param body logical. Whether or not to fetch the body of the resource.
#' @param soft logical. Whether or not to modify the cache to reflect
#'   the resource modification time and other details.
#' @param tracking logical. Whether or not to perform modification tracking
#'   by pushing accessed resources to the director's stack.
#' @param helper logical. If \code{TRUE}, allow processing of helper files.
# TODO: (RK) Explain idempotence and helpers more: https://github.com/robertzk/director/issues/23
#'   If a file shares its name with the parent directory (e.g., "foo"
#'   and "foo/foo.R"), it is called an idempotent resource. Any other files
#'   in the same directory as the idempotence resource, besides the file
#'   itself, are called helper files, and are usually invisible to the
#'   director object (e.g., "foo/other.R" if "foo/foo.R" exists).
#'
#'   If \code{helper = TRUE}, these will temporarily be treated as a
#'   resource so that we can track whether they were modified and re-use
#'   other \code{directorResource} features. By default, \code{helper = FALSE}.
#' @return A \code{\link{directorResource}} object.
resource <- function(name, ..., defining_environment. = parent.frame()) {
  force(defining_environment.)
  resource <- director_resource(self, resource_name(name), defining_environment.)
  process_resource(resource, ...)
}

