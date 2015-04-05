## Before reading this file, you should probably take a look at
## resource-modification_tracker.R.
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
#' @seealso \code{\link{active_resource}}, \code{\link{resource_caching}},
#'    \code{\link{tower}}
#' @return The parsed resource.
#' @note The local \code{any_dependencies_modified} rarely needs to be
#'    used by a preprocessor or parser. You should usually use 
#'    \code{resource caching} instead.
#'
#'    The parameters must be named \code{object} and \code{...} due to
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
#'   })
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
  if (identical(dependency_tracker.return, "any_dependencies_modified")) {
    any_dependencies_modified(object)
  } else if (identical(dependency_tracker.return, "dependencies")) {
    dependencies(object)
  } else {
    ## While a resource is being sourced it can reference other resources.
    ## For example, it could use the `resource(...)` function provided in
    ## the resource's sourcing environment. Its parser or preprocessor
    ## could call `director$resource(...)` directly to load another
    ## resource.
    ## 
    ## In other words, while the execution of this resource is on the
    ## call stack, whole swaths of other resources may be getting
    ## processed. To "remember" what resources have been referenced
    ## we will use some unfortunately necessary global state
    ## in the `begin_tracking_dependencies` helper, and then 
    ## undo our work in `stop_tracking_dependencies`.
    begin_tracking_dependencies(object)

    value <- yield()
    
    stop_tracking_dependencies(object)

    value
  }
}

dependency <- function(nesting_level, resource_name) {
  structure(class = "directorDependency", list(
    level = nesting_level,
    resource_name = resource_name
  ))
}

any_dependencies_modified <- function(active_resource) {
  ## If the resource has ever been parsed before, we will remember
  ## its dependencies in `state$dependency_tracker.dependencies`
  ## (as a character vector of resource names).
  dependencies <- active_resource$state$dependency_tracker.dependencies %||% character(0)

  ## Recall that the `modified` local was injected back in the
  ## `modification_tracker`.
  modified <- active_resource$injects$modified

  ## We recursively determine if this resource or any of its dependencies
  ## have been modified.
  is_modified <- function(name) {
    ## We have to set `modification_tracker.touch = FALSE` to not disturb
    ## the `modification_tracker.queue` -- this is a read-only operation
    ## and should not update any cached modification times!
    active_resource$resource$director$resource(name,
      modification_tracker.touch = FALSE,
      dependency_tracker.return  = "any_dependencies_modified"
    )
  }
  modified || any(vapply(dependencies, is_modified, logical(1)))
}

dependencies <- function(active_resource) {
  dependencies <- active_resource$state$dependency_tracker.dependencies %||% character(0)

  nested_dependencies <- lapply(
    dependencies,
    active_resource$resource$director$resource,
    modification_tracker.touch = FALSE,
    dependency_tracker.return  = "dependencies"
  )
  
  # Some resources may depend on the same dependencies, so we `unique`
  # at the end to jiggle those away.
  unique(c(recursive = TRUE, dependencies, nested_dependencies))
}

## This function is purely used for its side effects.
begin_tracking_dependencies <- function(active_resource) {
  director <- active_resource$resource$director

  ## First, we provide whether or not the resource or its dependencies
  ## *as of the last time this resource was executed* have been
  ## modified. (After all, we can't know a priori what its dependencies
  ## are before executing it.)
  any_modified <- director$resource(
    active_resource$resource$name,
    virtual_check.skip         = TRUE,
    dependency_tracker.return  = "any_dependencies_modified",
    modification_tracker.touch = FALSE
  )

  active_resource$injects %<<% list(any_dependencies_modified = any_modified)

  ## We will create a stack data structure to keep track of currently
  ## processed resources. Recall that `director_state` is an environment within
  ## the director package namespace that is used to explicitly represent
  ## state global to the R session.
  if (!base::exists("dependency_stack", envir = director_state)) {
    ## The "shtack" is defined in utils.R
    director_state$dependency_stack <- shtack$new()
  }

  ## The `nesting_level` will keep track of how far down the "resource
  ## call stack" we are. Every time we process a resource while amidst
  ## the processing of a parent resource, this number will get
  ## incremented by one.
  nesting_level <- director_state$dependency_nesting_level %||% 0
  if (nesting_level > 0L) {
    ## If this is not a top-level resource (i.e., one called directly
    ## using `director$resource` rather than from another resource),
    ## push this dependency onto the stack.
    ##
    ## We do not need to push top-level resources onto the stack
    ## since they aren't anyone's dependency!
    director_state$dependency_stack$push(
      dependency(nesting_level, active_resource$resource$name)
    )

    # TODO: (RK) Explain this tricky point.
    director_state$defining_environment <- 
      director_state$defining_environment %||%
      active_resource$resource$defining_environment
      
  } else {
    # TODO: (RK) Explain this tricky point.
    director_state$defining_environment <- NULL
  }

  director_state$dependency_nesting_level <- nesting_level + 1
}

stop_tracking_dependencies <- function(active_resource) {
  director <- active_resource$resource$director

  ## Recall the nesting level from `start_dependency_tracker`.
  ## It has been incremented by one until this method finishes.
  nesting_level <- director_state$dependency_nesting_level

  ## This is the key to determining a resource's immediate
  ## (as opposed to recursive) dependencies. Say we have
  ## "foo" which calls "bar" whichs calls "baz". The
  ## nesting level will get incremented once during the execution
  ## of "bar" and once again during "baz".
  ##
  ## Thus, we can tell "bar" is an immediate direct dependency
  ## of "foo" which "baz" is an implicit dependency through "bar"
  ## by noting the nesting level.
  dependencies <- Filter(
    function(dependency) dependency$level == nesting_level, 
    director_state$dependency_stack$peek(TRUE) # everything on the stack
  )

  ## If retrieved the resource from cache in the `caching_layer` we should
  ## not update its dependencies, since it will look like it has none.
  if (!isTRUE(active_resource$injects$cache_used)) {
    ## Now that we have snatched the correct dependencies in the previous
    ## statement, we just extract their proper resource names.
    active_resource$state$dependency_tracker.dependencies <-
      vapply(dependencies, getElement, character(1), "resource_name")
  }

  ## This is after all a dependency *stack*. We can remove all the
  ## dependencies of this resource by popping those with the current
  ## nesting level. Those with higher nesting levels will have been
  ## recursively popped off already.
  while (!director_state$dependency_stack$empty() &&
         director_state$dependency_stack$peek()$level == nesting_level) {
    director_state$dependency_stack$pop()
  }

  ## And we're back, cap'n.
  director_state$dependency_nesting_level <- nesting_level - 1
}
