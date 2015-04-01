# Minimalist persistent global state.
director_state <- new.env(parent = emptyenv())

director_resource <- function(director, name, defining_environment) {
  structure(list(
    director = director,
    name = name,
    defining_environment = defining_environment
  ), class = "director_resource")
}

# Construct a resource-compiling tower.
process_resource <- function(resource, ...) {
  enforce_type(resource, "director_resource", "process_resource")

  tower(
    virtual_check        %>>%
    modification_tracker %>>%
    dependency_tracker   %>>% 
    caching_layer        %>>%
    preprocessor         %>>%
    parser               
  )(active_resource(resource), ...)
}

# An active resource is just a list that holds a resource,
# but also an "injects" environment and "state", which is
# like the equivalent of the Haskell IO monad.
active_resource <- function(resource) {
  structure(class = "active_resource", list(
    resource = resource,
    injects  = new.env(parent = topenv(resource$defining_environment)),
    state    = generate_state(resource)
  ))
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
