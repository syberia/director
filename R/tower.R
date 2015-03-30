#' Create a tower of functions.
#'
#' A tower is equivalent to the Ruby gem Rack's notion of middleware.
#'
#' Imagine a function \code{f1} that, in the middle of its processing,
#' calls another function, \code{f2}, which in the middle of its
#' processing, calls another function, \code{f3}, and so on.
#' 
#' To construct such a structure from the list of functions
#' \code{list(f1, f2, ...)} we need to be able to call the 
#' next "inner" function from the previous outer function.
#'
#' This is accomplished by providing a \code{yield} keyword
#' which simply calls the next function in the tower.
#'
#' The purpose of a tower is to modify some primary object
#' throughout its operation. To this end, an \code{object} keyword
#' will be provided to each tower function. If \code{object} is
#' modified prior to a \code{yield} call, the next function in
#' the tower will receive the modified object.
#'
#' For composability, every function in a tower should have a
#' \code{yield} keyword. The last function in the tower will
#' yield to an identity function that simply returns the \code{object}.
#'
#' @param functions list. A list of functions in the tower.
#' @return An S3 "tower" object, which is a callable function
#'    and must be passed the \code{object} as the first argument.
#'    Additional arguments will be passed to the first function
#'    in the tower.
tower <- function(functions) {
}
