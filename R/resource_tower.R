
# director$resource <- function(name, ..., defining_environment. = parent.frame())  {
#   name <- resource_name(name)
#   force(defining_environment.)
#   resource <- resource_class(self, name, defining_environment.)
#   # Apply tower
# }

# Minimalist persistent global state.
director_state <- new.env(parent = emptyenv())

# Construct a resource-compiling tower.
resource_tower <- function(director, name, ...) {
  # This is the dream! Now we have to make it happen.

  resource_class <- function(director, name, defining_environment = parent.frame()) {
    structure(list(director = director, name = name,
                   defining_environment = defining_environment),
              class = "director_resource")
  }
  resource <- resource_class(director, name, parent.frame())

  virtual_check        %>>%
  modification_tracker %>>%
  dependency_tracker   %>>% 
  # caching_layer        %>>%
  # preprocessor         %>>%
  # parser               %>>%
  as.active_resource(resource)
}

# An active resource is just a list that holds a resource,
# but also an "injects" environment and "state", which is
# like the equivalent of the Haskell IO monad.
as.active_resource <- function(resource) {
  list(
    resource = resource,
    injects  = new.env(parent = topenv(resource$defining_environment)),
    state    = generate_state(resource)
  )
}

# Generate the persistent global state for a resource.
generate_state <- function(resource) {
  # TODO: (RK) Issue warnings when directors on the same directory with the
  # same project name are instantiated, as they will conflict with each
  # others' global state: https://github.com/robertzk/director/issues/25
  director_key <- (function(director) {
    digest::digest(list(director$root(), director$project_name()))
  })(resource$director)

  ## We do not need `inherits = FALSE` because the parent environment is
  ## the empty environment.
  if (!base::exists(director_key, envir = director_state)) {
    director_state[[director_key]] <- new.env(parent = emptyenv())
  }
  state <- director_state[[director_key]]

  if (!base::exists(resource$name, envir = state)) {
    state[[resource$name]] <- new.env(parent = emptyenv())
  }
  state[[resource$name]]
}

virtual_check <- function(object, ...) {
  director <- object$resource$director
  virtual  <- !director$exists(object$resource$name)
  object$injects %<<% list(virtual = virtual)
  
  if (virtual && !director$has_preprocessor(object$resource$name)) {
    project_name <- director$project_name()
    stop(sprintf("Cannot find resource %s, in%s project %s.",
      sQuote(crayon::red(object$resource$name)),
      if (nzchar(project_name)) paste0(" ", project_name) else "",
      sQuote(crayon::blue(director$root()))))
  }

  yield()
}

modification_tracker <- function(object, ..., modification_tracker.return = "object",
                                 modification_tracker.touch = TRUE) {
  director <- object$resource$director

  if (isTRUE(object$injects$virtual)) {
    ## Virtual resources are always considered to be modified, since we have
    ## no way to tell.
    object$injects %<<% list(modified = TRUE)
    yield()
  } else {
    if (!base::exists("modification.queue", envir = object$state)) {
      object$state$modification.queue <- sized_queue(size = 2)
    }

    modified <- function() {
      ## A resource has been modified if its modification time has changed. 
      !do.call(identical, lapply(seq(2), object$state$modification.queue$get))
    }

    if (isTRUE(modification_tracker.touch)) {
      filename <- director$filename(object$resource$name, enclosing = TRUE)
      mtime    <- file.info(filename)$mtime
      object$state$modification.queue$push(mtime)
      object$injects %<<% list(modified = modified())
    }

    if (identical(modification_tracker.return, "modified")) {
      modified()
    } else {
      yield()
    }
    # TODO: (RK) Set any_dependencies_modified on exit
  }
}

dependency_tracker <- function(object, ...) {
  director <- object$resource$director

  if (!base::exists("dependency_stack", envir = director_state)) {
    director_state$dependency_stack <- shtack$new()
  }

  nesting_level <- director_state$dependency_nesting_level %||% 0
  if (nesting_level > 0L) {
    director_state$dependency_stack$push(
      dependency(nesting_level + 1, object$resource$name)
    )
  } else {
    director_state$dependency_stack$clear()
  }
  director_state$dependency_nesting_level <- nesting_level + 1

  object <- yield()

  director_state$dependency_nesting_level <- nesting_level - 1
  dependencies <- Filter(
    function(dependency) dependency$level == nesting_level + 1, 
    director_state$dependency_stack$peek(TRUE)
  )

  any_modified <- any(vapply(dependencies, function(d) {
    resource_tower(director, d$name, modification_tracker.touch = FALSE,
                   modification_tracker.return = "modified")
  }, logical(1)))

  object$injects %<<% list(any_dependencies_modified = any_modified)

  while (!director_state$dependency_stack$empty() &&
         director_state$dependency_stack$peek()$level == nesting_level + 1) {
    director$dependency_stack$pop()
  }

  yield()
}

dependency <- function(nesting_level, resource_name) {
  structure(class = "directorDependency", list(
    level = nesting_level, resource_name = resource_name
  ))
}



## Virtual check:
# - Check if virtual resource, inject "virtual"
# - Error if no preprocessor exists (but should that be here or down there?)                  
## Modification tracker:
# - Compute modification info. Insert into injects. Insert into state iff soft. != FALSE
# - If modification detected (on the directory level),
#   inject "modified" to be TRUE, otherwise FALSE. Note we no longer need
#   special behavior for idempotent resources since we are looking on the
#   directory level                  
# - on.exit, set "any_dependencies_modified" to be modified || any dependencies modified.
## Dependency tracker:
# - Inject dependencies from cached state.
# - Start dependency tracking + nesting level.
# - on.exit, place just-determined dependencies in state.
## Caching layer:
# - If caching is enabled (ask the director) and cached value is in state,
#   just use it an return here, unless recompile. = TRUE
# - If caching is enabled (ask the director) then on.exit place the computed
#   value in *state*.
## Preprocessor 
# - Find preprocessor using director, throw in a ton of injects, send to preprocessor.
#   TODO: How to pass preprocessed value to parser?
#   If parse. = FALSE, just return preprocessed value.
## Parser
# - Find parser using director, throw in a ton of injects, send to parser.
#   Return final output instead of yielding.
