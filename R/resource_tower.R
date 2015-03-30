
# director$resource <- function(name, ..., defining_environment. = parent.frame())  {
#   name <- resource_name(name)
#   force(defining_environment.)
#   resource <- resource_class(self, name, defining_environment.)
#   # Apply tower
# }

# Construct a resource-compiling tower.
resource_tower <- function(director, name) {
  # This is the dream! Now we have to make it happen.

  resource_class <- function(director, name) {
    structure(list(director = director, name = name), class = "director_resource")
  }
  resource <- resource_class(director, name)

  # virtual_check        %>>%
  # modification_tracker %>>%
  # dependency_tracker   %>>% 
  # caching_layer        %>>%
  # preprocessor         %>>%
  # parser               %>>%
  # as.active_resource(resource)
}

# An active resource is just a list that holds a resource,
# but also an "injects" environment and "state", which is
# like the equivalent of the Haskell IO monad.
as.active_resource <- function(resource) {
  list(
    resource = resource,
    injects  = new.env(parent = topenv(resource$defining_environment)),
    state    = NULL # TODO: (RK) Figure out what kind of thing this is.
  )
}

virtual_check <- function(object, ...) {
  director <- object$resource$director
  virtual <- !director$exists(object$resource$name)
  object$inject %<<% list(virtual = virtual)
  
  if (virtual && !director$has_preprocessor(object$resource$name)) {
    project_name <- director$project_name()
    stop(sprintf("Cannot find resource %s, in%s project %s.",
      sQuote(crayon::red(object$resource$name)),
      if (nzchar(project_name)) paste0(" ", project_name) else "",
      sQuote(crayon::blue(director$root()))))
  }

  yield()
}

modification_tracker <- function(object, ..., modification_tracker.return = "object") {

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
