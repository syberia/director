#' Cache a resource's parsed value.
#'
#' More complex resources are typically time-consuming to compile, or
#' are unnecessary to compile more than once per R session.
#'
#' The \code{caching_layer} provides an internal caching mechanism that
#' will remember the value of the parsed resource and re-use it
#' \emph{unless the resource or any of its dependencies have been
#' modified} (see \link{dependency_tracking} for an explanation of
#' how this is accomplished).
#'
#' @name resource caching
#' @aliases caching_layer
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
#' @param recompile. logical. Whether or not to force the resource to
#'    be recompiled (instead of retrieved from cache), regardless of
#'    whether the resource or any of its dependencies have been modified.
#' @seealso \code{\link{active_resource}}
#' @return The parsed resource, retrieved from cache since the last time
#'    the resource was executed if and only if the resource's file(s) and
#'    none of its (recursive) dependencies have been modified.
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
#'   d$register_parser("runners/", function(output) {
#'     stagerunner::stageRunner$new(new.env(), output)
#'   }, cache = TRUE) # Note the cache = TRUE argument.
#'
#'   sr  <- d$resource("runners/project1") # A fresh new stageRunner!
#'   sr2 <- d$resource("runners/project1") # Same one, since it used the cache.
#'   stopifnot(identical(sr, sr2))
#'
#'   # We can use base::Sys.setFileTime to pretend like we updated the
#'   # modified time of the /dir/connections/dev.R file, triggering
#'   # the caching layer to re-compile the resource.
#'   Sys.setFileTime(file.path(d$root(), "runners", "project1.R"),
#'     Sys.time() - as.difftime(1, units = "mins"))
#'
#'   sr3 <- d$resource("runners/project1") # Now it re-builds the runner.
#'   stopifnot(!identical(sr, sr3)) # A new runner, with hardly any work!
#' }
caching_layer <- function(object, ..., recompile. = FALSE) {
  caching_enabled <- any_is_substring_of(object$resource$name,
    object$resource$director$cached_resources())

  if (!caching_enabled) {
    yield()
  } else {
    ## If this resource has been parsed before but any of its dependencies
    ## have been modified, we should wipe the cache.
    is_cached <- 
      !isTRUE(recompile.) && 
      !isTRUE(object$injects$any_dependencies_modified) &&
      base::exists("caching_layer.value", envir = object$state)

    if (is_cached) {
      object$injects$cache_used <- TRUE
      object$state$caching_layer.value
    } else {
      (object$state$caching_layer.value <- yield())
    }
  }
}
