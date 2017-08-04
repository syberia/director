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
  UseMethod("process_resource")
}

## If `resource` is a character, we assume it refers to a filename.
process_resource.process_resource <- function(resource, ...) {
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
    # digest::digest(list(director$root(), director$project_name()))
    paste0("director", director$.id)
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

