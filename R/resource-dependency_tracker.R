#' Track the dependencies of a resource.
#'
#' Virtual resources are those that are not recorded as a .R file. Instead,
#' the resource's value must be computed using a preprocessor.
#'
#' For example, imagine we have a directory of resources where some of the
#' resources have been re-factored into a package. We would still like to be
#' able to turn objects from that package into proper resources, but they
#' may no longer be encoded as files in the Syberia project.
#'
#' Instead, we could define a preprocessor that looks for those special values
#' and uses the package objects instead.
#'
#' When parsing a resource, the local \code{virtual} is injected for use in
#' the preprocessor which corresponds to whether the resource seems
#' non-existent to the director (i.e., has no supporting .R file).
#'
#' @name modification tracking
#' @aliases modification_tracker
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
#' @param modification_tracker.return character. What to return in this layer
#'    of the parsing tower. The options are \code{c("modified", "object")}.
#'
#'    The former returns whether or not the file associated with the resource
#'    has been modified (or in the case of idempotent resources, the file and
#'    its helpers). The resource itself will not be parsed.
#'
#'    The latter, \code{"object"}, will parse the resource as usual. This is
#'    the default.
#' @param modification_tracker.touch logical. Whether or not to update an
#'    internal cache that keeps track of last time the resource is modified.
#'    This is an internal parameter that is set to \code{FALSE} by recursive
#'    calls to \code{director$resource} to avoid polluting the modification
#'    cache while investigating dependency changes. The default is \code{TRUE}.
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
#'   #=== R console ===
#'   d <- director("/dir") # Create a director object.
#'   d$register_preprocessor("runners/", function(director, source, modified) {
#'     # `modified` has been set by the modification_tracker to
#'     # TRUE or FALSE according as /dir/runners/project1.R has been modified.
#'     if (modified || is.null(runner <- director$cache_get("last_runner"))) {
#'       # Construct a new stageRunner, since the file has been modified.
#'       source()
#'     } else { runner }
#'   }
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
#'   # modified time of the project1.R file, triggering `modified = TRUE`.
#'   Sys.setFileTime(file.path(d$root(), "runners", "project1.R"),
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
