#' Track the dependencies of a resource.
#'
#' More complex resources are often built from simpler resources. It is 
#' the responsibility of the \code{dependency_tracker} to determine
#' whether any dependencies have been modified.
#'
#' The \code{dependency_tracker} is very good at its job and can track
#' arbitrarily nested dependencies (for example, if resource \code{"foo"}
#' needs resource \code{"bar"} who needs resource \code{"baz"}, etc.).
#' But beware! The \code{dependency_tracker} won't tolerate circular
#' dependencies with anything except tears of anguish.
#'
#' The local \code{any_dependencies_modified} is injected by the 
#' \code{dependency_tracker} for use in the preprocessor or parser
#' of a resource. Note this is based off the dependencies \emph{last time}
#' the resource was executed, since it is impossible to know a priori
#' what the dependencies will be prior to sourcing the resource's file.
#'
#' The local \code{dependencies}, a character vector of (recursive)
#' dependencies is also injected.
#'
#' @name dependency tracking
#' @aliases dependency_tracker
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
#' @param dependency_tracker.return. What to return in this layer
#'    of the parsing tower. The options are \code{"dependencies"},
#'    \code{"any_dependencies_modified"}, and \code{"object"}.
#'  
#'    The former returns the list of recursive dependencies of the resource,
#'    as of last time the resource was executed.
#'   
#'    Choosing \code{"any_dependencies_modified"} will answer whether any
#'    of the files associated with the dependencies, \emph{or the resource
#'    itself}, have been modified.
#'
#'    The last (default) choice, \code{"object"}, will return the parsed
#'    resource's value as usual by proceeding with the resource parsing
#'    tower.
#' @seealso \code{\link{active_resource}}
#' @return The parsed resource.
#' @note The parameters must be named \code{object} and \code{...} due to
#'    this method's inclusion in a \code{\link{tower}}.
#' @examples
#' \dontrun{
#'   # Imagine we are constructing a stagerunner from a sequence of functions.
#'   # However, some of those functions have been built by other resources.
#'   # Imagine the following structure.
#'   # (See github.com/robertzk/stagerunner for an explanation of stagerunners.)
#'
#'   #=== /dir/runners/project1.R ===
#'   list(
#'     "import data"  = resource("importers/db"),   # These are some functions
#'     "munge data"   = resource("mungers/impute"), # built by the user
#'     "create model" = resource("models/lm"),      # that live in other
#'     "export model" = resource("exporters/file")  # files.
#'   )
#'
#'   #=== /dir/importers/db.R ===
#'   conn <- resource("connections/dev") # A list representing a connection
#'     # setting to a development database.
#'   DBI::dbReadTable(conn, "some_table")
#'
#'   #=== /dir/connections/dev.R
#    # Some file that sets up and returns a database connection.
#'
#'   #=== R console ===
#'   d <- director("/dir") # Create a director object.
#'   d$register_preprocessor("runners/",
#'     function(director, source, any_dependencies_modified) {
#'       # `any_dependencies_modified` has been set by the dependency_tracker to
#'       # TRUE or FALSE according as /dir/runners/project1.R *or any of its
#'       # dependencies* has been modified.
#'       if (any_dependencies_modified ||
#'           is.null(runner <- director$cache_get("last_runner"))) {
#'         # Construct a new stageRunner, since a dependency has been modified.
#'         source()
#'       } else { runner }
#'   })
#'
#'   d$register_parser("runners/", function(output) {
#'     # If it is a stageRunner, it must have been retrieved from the cache.
#'     if (stagerunner::is.stageRunner(output)) { return(output) }
#'     runner <- stagerunner::stageRunner$new(new.env(), output)
#'  
#'     # Cache the runner so it is available in the preprocessor next time.
#'     # As long as the /dir/runners/project1.R file remains untouched, we will
#'     # not have to bother re-sourcing the file and hence reconstructing the
#'     # stageRunner.
#'     director$cache_set("last_runner", runner)
#'     runner
#'   }
#'
#'   sr  <- d$resource("runners/project1") # A fresh new stageRunner!
#'   sr2 <- d$resource("runners/project1") # Same one, since it used the cache.
#'   stopifnot(identical(sr, sr2))
#'
#'   # We can use base::Sys.setFileTime to pretend like we updated the
#'   # modified time of the /dir/connections/dev.R file, triggering
#'   # `any_dependencies_modified = TRUE`.
#'   Sys.setFileTime(file.path(d$root(), "connections", "dev.R"),
#'     Sys.time() - as.difftime(1, units = "mins"))
#'
#'   sr3 <- d$resource("runners/project1") # Now it re-builds the runner.
#'   stopifnot(!identical(sr, sr3)) # A new runner!
#' }
dependency_tracker <- function(object, ..., dependency_tracker.return = "object") {
  director <- object$resource$director

  if (identical(dependency_tracker.return, "any_dependencies_modified")) {
    dependencies <- object$state$dependency_tracker.dependencies %||% character(0)
    modified <- object$injects$modified
    is_modified <- function(name) {
      object$resource$director$resource(name, modification_tracker.touch = FALSE,
        dependency_tracker.return = "any_dependencies_modified")
    }
    return(modified || any(vapply(dependencies, is_modified, logical(1))))
  } else if (identical(dependency_tracker.return, "dependencies")) {
    dependencies <- object$state$dependency_tracker.dependencies %||% character(0)
    nested_dependencies <- lapply(
      dependencies,
      director$resource,
      modification_tracker.touch = FALSE,
      dependency_tracker.return  = "dependencies"
    )
    # TODO: (RK) Figure out why we need unique, nested dependencies should not need it.
    return(unique(c(recursive = TRUE, dependencies, nested_dependencies)))
  }

  any_modified <- director$resource(object$resource$name,
    dependency_tracker.return = "any_dependencies_modified",
    modification_tracker.touch = FALSE)
  object$injects %<<% list(any_dependencies_modified = any_modified)

  if (!base::exists("dependency_stack", envir = director_state)) {
    director_state$dependency_stack <- shtack$new()
  }

  nesting_level <- director_state$dependency_nesting_level %||% 0
  if (nesting_level > 0L) {
    director_state$dependency_stack$push(
      dependency(nesting_level, object$resource$name)
    )
  } else {
    director_state$dependency_stack$clear()
  }
  director_state$dependency_nesting_level <- nesting_level + 1

  value <- yield()

  director_state$dependency_nesting_level <- nesting_level
  dependencies <- Filter(
    function(dependency) dependency$level == nesting_level + 1, 
    director_state$dependency_stack$peek(TRUE)
  )
  if (!isTRUE(object$injects$cache_used)) {
    object$state$dependency_tracker.dependencies <-
      vapply(dependencies, getElement, character(1), "resource_name")
  }

  # TODO: (RK) This is incorrect, figure out right dependency modification check
  any_modified <- any(vapply(dependencies, function(d) {
    director$resource(d$resource_name, modification_tracker.touch = FALSE,
                   modification_tracker.return = "modified")
  }, logical(1)))

  while (!director_state$dependency_stack$empty() &&
         director_state$dependency_stack$peek()$level == nesting_level + 1) {
    director_state$dependency_stack$pop()
  }
  
  value
}
