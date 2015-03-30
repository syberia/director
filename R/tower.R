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
#' @param functions list. A list of functions in the tower. The
#'    first argument of each function must be named "object" and
#'    each function must take a \code{...} parameter. By default
#'    \code{list()}, which creates an identity tower that performs
#'    no operation.
#' @return An S3 "tower" object, which is a callable function
#'    and must be passed the \code{object} as the first argument.
#'    Additional arguments will be passed to the first function
#'    in the tower.
#' @export
#' @examples
#' functions <- list(
#'   function(object, ...) {
#'     object <- object + 1
#'     object <- yield()
#'     object + 1
#'   },
#'
#'   function(object, ...) {
#'     object <- object * 2
#'     yield()
#'   }
#' )
#'
#' t <- tower(functions)
#' v <- t(1) # This is now 5, since in the tower, we increment object,
#'           # Multiply it by 2 in the next function, then increment it
#'           # again after receiving the previous function.
#' stopifnot(v == 5)
tower <- function(functions = list()) {
  stopifnot(is.list(functions),
            all(sapply(functions, is.function)))

  verify_function <- function(fn) {
    formal_names <- names(formals(fn))
    stopifnot(identical(formal_names[1], "object"))
    stopifnot(is.element("...", formal_names))
    stopifnot(is.identity2(fn) || is.element("yield", all.names(body(fn))))
  }
  lapply(functions, verify_function)

  inject <- function(fn) {
    yield <- function() {
      eval.parent(quote(
        `*NEXT.FUN*`(object, ...)
      ))
    }

    fn <- duplicate(fn)
    environment(fn) <- list2env(list(
      yield = yield
    ), parent = environment(fn))
    fn
  }

  # Inject the yield keyword to each function.
  functions <- lapply(functions, inject)
  functions[[length(functions) + 1]] <- identity2

  # Provide the next function.
  for (i in seq_len(length(functions) - 1)) {
    environment(functions[[i]])$`*NEXT.FUN*` <- functions[[i + 1]]
  }

  structure(functions[[1]], class = "tower")
}

`%>>%` <- function(lhs, rhs) {
  merge <- function(lhs, rhs) {
    # TODO: (RK) Use better heuristic here.
    if (!is.function(rhs)) {
      tower(as.pre_tower(lhs))(rhs)
    } else {
      as.pre_tower(list(lhs, rhs))
    }
  }

  merge(lhs, rhs)
}

as.pre_tower <- function(fn) {
  structure(c(recursive = TRUE, fn), class = "pre_tower")
}
is.pre_tower <- function(obj) is(obj, "pre_tower")

identity2 <- structure(function(object, ...) object, class = "identity")
is.identity2 <- function(x) is(x, "identity")

