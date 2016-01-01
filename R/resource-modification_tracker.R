## Take a look at resource-virtual_checker.R prior to reading this file.
#' Detect modifications to a resource.
#'
#' The \code{modification_tracker} is responsible for determining whether
#' any changes have been made to the file(s) associated to the given
#' resource.
#'
#' If a modification has been detected, the local \code{modified}
#' will be injected for use in the preprocessor or parser for the resource.
#'
#' Note that we can use the \code{modification_tracker} to determine
#' whether the resource has been modified:
#'
#' \code{director_object$resource(resource_name,
#'   modification_tracker.touch = FALSE,
#'   modification_tracker.return = "modified")}
#' 
#' The use of \code{modification_tracker.touch = FALSE} is necessary to avoid
#' polluting the internal cache that determines whether or not the resource
#' has been modified.
#' 
#' @name modification tracking
#' @aliases modification_tracker
#' @param object active_resource. See \code{\link{active_resource}}.
#' @param ... additional parameters to pass to the next layer in the resource
#'    parsing tower.
## The incorporation of these parameters is a little suspect, but I saw no
## no other way to retain the elegance and composability of the tower
## abstraction while providing the ability to fetch information from
## "halfway down the stream." 
##
## A possible alternative is to implement the equivalent of Scala's "Breaks"
## by exploiting the R condition system.
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
#' @seealso \code{\link{active_resource}}, \code{\link{tower}}
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
#'   })
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

  ## We injected `virtual` in the previous layer, `virtual_checker`, using 
  ## `object$injects %<<% list(virtual = virtual)`
  if (isTRUE(object$injects$virtual)) {
    ## Virtual resources are never considered to be modified, since we have
    ## have no corresponding file and we have no way to tell.
    if (identical(modification_tracker.return, "modified")) {
      FALSE
    } else if (identical(modification_tracker.return, "mtime")) {
      NULL
    } else {
      object$injects %<<% list(modified = FALSE)
      yield()
    }
  } else {
    ## In order to keep track of whether the resource has been modified,
    ## we will need to store the previous and current modification time
    ## and compare them to see if anything has changed.
    ##
    ## To this end, we make use of a `sized_queue`, a queue (in this
    ## case of length 2) where pushing a new element pops off the last
    ## if there are more than 2. This may seem extravagant, but will
    ## make it trivial to extend director to keep track of longer modification
    ## histories if we ever need to do so.
    ##
    ## We use `base::exists` instead of `exists` to make it clear we are
    ## not calling `exists` on the `director` object.
    if (!base::exists("modification_tracker.queue", envir = object$state)) {
      object$state$modification_tracker.queue <- sized_queue(size = 2)
    }

    mtime <- determine_last_modified_time(object)

    ## The `modification_tracker.touch` argument says we would like to
    ## explicitly modify the allocated queue of modification times.
    ## This means that we will have to wait until the resource's file changes
    ## again to mark it as modified.
    if (isTRUE(modification_tracker.touch)) {
      object$state$modification_tracker.queue$push(mtime)
      ## In this case, a resource has been modified if its modification time
      ## is not the same as last time, i.e., the first and second element in
      ## the queue are not identical.
      modified <- !do.call(identical,
        lapply(seq(2), object$state$modification_tracker.queue$get)
      )
    } else {
      modified <- !identical(object$state$modification_tracker.queue$get(1), mtime)
    }

    # Ferry whether or not the resource has been modified to the preprocessor
    # and parser down the stream.
    object$injects %<<% list(modified = modified)

    if (identical(modification_tracker.return, "modified")) {
      object$injects$modified
    } else if (identical(modification_tracker.return, "mtime")) {
      mtime
    } else {
      yield()
    }
  }
}

determine_last_modified_time <- function(active_resource) {
  director <- active_resource$resource$director

  ## It only makes sense to check for modifications (and thus query the
  ## file system) if the current time is later than the last modification
  ## time--otherwise, the file could not possibly have been updated yet.
  ##
  ## The operator `%|||%` is defined in utils.R and means "if the former 
  ## argument is `NULL` or `NA`, use the latter instead."
  if (Sys.time() > active_resource$state$modification_tracker.queue$get(1) %|||% 0) {
    ## We use the `director$filename` method to obtain the file associated
    ## with the resource. By setting `enclosing = TRUE`, we ensure that
    ## in the case of idempotent resources this will be directory instead
    ## of the .R file (e.g. "foo" instead of "foo/foo.R").
    filename <- director$filename(active_resource$resource$name, check.exists = FALSE,
                                  absolute = TRUE, enclosing = TRUE)
    if (is.idempotent_directory(filename)) {
      ## We use the `get_helpers` function in utils.R to get all
      ## the helper files. If the resource file ("foo/foo.R") or its
      ## helpers ("foo/helper.R", etc) have been modified, it will be
      ## reflected in the maximum of their modification times.
      files <- c(filename, get_helpers(filename, full.names = TRUE, leave_idempotent = TRUE))
      mtime <- max(file.info(files)$mtime, na.rm = TRUE)
    } else {
      mtime <- file.info(filename)$mtime
    }
  } else {
    ## In this case, the current timestamp is <= the last modified time,
    ## so nothing could have possibly changed yet. We use the cached
    ## modification time from the queue defined earlier.
    mtime <- active_resource$state$modification_tracker.queue$get(1)
  }

  mtime
}

