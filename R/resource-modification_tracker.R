#' Detect modifications to a resource.
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
#' @param object active_resource.
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
modification_tracker <- function(object, ..., modification_tracker.return = "object",
                                 modification_tracker.touch = TRUE) {
  director <- object$resource$director

  if (isTRUE(object$injects$virtual)) {
    ## Virtual resources are always considered to be modified, since we have
    ## no way to tell.
    object$injects %<<% list(modified = TRUE)
    yield()
  } else {
    if (!base::exists("modification_tracker.queue", envir = object$state)) {
      object$state$modification_tracker.queue <- sized_queue(size = 2)
    }

    if (Sys.time() > object$state$modification_tracker.queue$get(1) %||% 0) {
      # Directory modification is only defined as adding files.
      filename <- director$filename(object$resource$name,
                                    absolute = TRUE, enclosing = TRUE)
      if (is.idempotent_directory(filename)) {
        files <- c(filename, get_helpers(filename, full.names = TRUE, leave_idempotent = TRUE))
        mtime <- max(file.info(files)$mtime, na.rm = TRUE)
      } else {
        mtime <- file.info(filename)$mtime
      }
    } else {
      # The current timestamp is <= the last modified time, so nothing could
      # have possibly changed yet.
      mtime <- object$state$modification_tracker.queue$get(1)
    }

    if (isTRUE(modification_tracker.touch)) {
      object$state$modification_tracker.queue$push(mtime)
      modified <- function() {
        ## A resource has been modified if its modification time has changed. 
        !do.call(identical, lapply(seq(2), object$state$modification_tracker.queue$get))
      }
    } else {
      modified <- function() { !identical(object$state$modification_tracker.queue$get(1), mtime) }
    }

    object$injects %<<% list(modified = modified())

    if (identical(modification_tracker.return, "modified")) {
      object$injects$modified
    } else if (identical(modification_tracker.return, "mtime")) {
      object$state$modification_tracker.queue$get(1)
    } else {
      yield()
    }
    # TODO: (RK) Set any_dependencies_modified on exit
  }
}
