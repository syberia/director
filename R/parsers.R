#' Register a resource parser.
#'
#' @param path character. The prefix to look for in the director.
#' @param parser directorParser. 
register_parser <- function(path, parser) {
  stopifnot(is.character(path))
  stopifnot(is.function(parser))
  #stopifnot(length(formals(parser)) == 
}

