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

  ## Find the character string representing the preprocessor route used
  ## for preprocessing the resource.
  route <- director$match_preprocessor(object$resource$name)

  if (isTRUE(object$injects$virtual)) {
    ## Virtual resources are by definition those with no corresponding
    ## filename. See the `virtual_checker`.
    filename <- NULL
  } else {
    ## We place the filename in the object's injects to make it
    ## available to the `parser` down the stream.
    filename <- object$injects$filename <-
      normalizePath(director$filename(object$resource$name,
                                      absolute = TRUE, check.exists = FALSE))
  }

  object$injects %<<% list(
    # TODO: (RK) Use alist so these aren't evaluated right away.
    root = director$root,
    # TODO: (RK) Use find_director helper to go off root + project_name
    # TODO: (RK) Determine how to handle defining_environment. problems here.
    resource = function(...) {
      defining_environment <- parent.frame()
      director$resource(..., defining_environment. = defining_environment)
    },
    resource_name = object$resource$name,
    resource_exists = function(...) director$exists(...),
    helper = function(...) {
      director$resource(..., parse. = FALSE, virtual_check.skip = TRUE)
    }
  )

  if (is.null(route)) {
    object$preprocessed <- default_preprocessor(object) 
  } else {
    object$preprocessed <- apply_preprocessor_route(object, route, list(...))
  }

  if (isTRUE(parse.)) {
    yield() # Apply parser.
  } else {
    object$preprocessed$value
  }
}

default_preprocessor <- function(active_resource) {
  # There is no preprocessor for this resource, so we
  # use the default preprocessor, base::source.
  default_preprocessor_fn <- function(filename) {
    # TODO: (RK) Figure out correct environment assignment.
    base::source(filename, local = source_env, keep.source = TRUE)$value
  }

  parent_env <- active_resource$resource$defining_environment
  if (!identical(topenv(parent_env), baseenv())) {
    parent_env <- parent.env(topenv(parent_env))
  } else {
    parent_env <- parent.env(parent_env)
  }

  source_env <- new.env(parent = parent_env)
  # source_env <- new.env(parent = parent.env(topenv(parent.env(environment()))))
  source_env %<<% active_resource$injects
  active_resource$state$preprocessor.source_env <- source_env

  environment(default_preprocessor) <- new.env(
    parent = active_resource$resource$defining_environment
  ) %<<% list(source_env = source_env)

  list(
    value               = default_preprocessor_fn(active_resource$injects$filename),
    preprocessor_output = new.env(parent = emptyenv())
  )
}

apply_preprocessor_route <- function(active_resource, route, args, filename) {
  director <- active_resource$resource$director

  active_resource$state$preprocessor.source_env <- new.env(parent = active_resource$injects)

  preprocessor_output <- new.env(parent = emptyenv())
  preprocessor_function <- director$preprocessor(route)

  sourcing_env <-
    new.env(parent = director_state$defining_environment) %<<%
    environment(preprocessor_function)

  environment(preprocessor_function) <-
    new.env(parent = sourcing_env) %<<%
    active_resource$injects %<<% list(
      # TODO: (RK) Intersect with preprocessor formals.
      # TODO: (RK) Use alist so these aren't evaluated right away.
       resource = active_resource$resource$name,
       director = director,
       args = args,
       filename = active_resource$injects$filename,
       source_env = active_resource$state$preprocessor.source_env,
       source = function() eval.parent(quote({
        if (!is.character(filename)) {
          stop("Director of project ", sQuote(crayon::yellow(director$root())),
               " attempted to source filename of class ", class(filename)[1L], call. = FALSE)
        }
        if (!file.exists(filename)) {
          stop("Director of project ", sQuote(crayon::yellow(director$root())),
               " attempted to source ", sQuote(crayon::red(filename)),
               ", but this file does not exist.", call. = FALSE)
        }
        base::source(filename, source_env, keep.source = TRUE)$value
       })),
       preprocessor_output = preprocessor_output,
       "%||%" = function(x, y) if (is.null(x)) y else x
    )

  list(
    value = preprocessor_function(),
    preprocessor_output = preprocessor_output
  )
}

