
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
  resource_class <- function(director, name, defining_environment = parent.frame()) {
    structure(list(director = director, name = name,
                   defining_environment = defining_environment),
              class = "director_resource")
  }
  resource <- resource_class(director, name, parent.frame())

  tower(
    virtual_check        %>>%
    modification_tracker %>>%
    dependency_tracker   %>>% 
    caching_layer        %>>%
    preprocessor         %>>%
    parser               
  )(as.active_resource(resource), ...)
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
    if (!base::exists("modification_tracker.queue", envir = object$state)) {
      object$state$modification_tracker.queue <- sized_queue(size = 2)
    }

    modified <- function() {
      ## A resource has been modified if its modification time has changed. 
      !do.call(identical, lapply(seq(2), object$state$modification_tracker.queue$get))
    }

    if (isTRUE(modification_tracker.touch)) {
      # Directory modification is only defined as adding files.
      filename <- director$filename(object$resource$name,
                                    absolute = TRUE, enclosing = TRUE)
      if (is.idempotent_directory(filename)) {
        files <- c(filename, get_helpers(filename, full.names = TRUE, leave_idempotent = TRUE))
        mtime <- max(file.info(files)$mtime, na.rm = TRUE)
      } else {
        mtime <- file.info(filename)$mtime
      }
      object$state$modification_tracker.queue$push(mtime)
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
  }

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
  object$state$dependency_tracker.dependencies <-
    vapply(dependencies, getElement, character(1), "resource_name")

  # TODO: (RK) This is incorrect, figure out right dependency modification check
  any_modified <- any(vapply(dependencies, function(d) {
    resource_tower(director, d$resource_name, modification_tracker.touch = FALSE,
                   modification_tracker.return = "modified")
  }, logical(1)))

  object$injects %<<% list(any_dependencies_modified = any_modified)

  while (!director_state$dependency_stack$empty() &&
         director_state$dependency_stack$peek()$level == nesting_level + 1) {
    director_state$dependency_stack$pop()
  }
  
  value
}

dependency <- function(nesting_level, resource_name) {
  structure(class = "directorDependency", list(
    level = nesting_level, resource_name = resource_name
  ))
}

# If recompile. = TRUE, the caching layer will always be ignored.
caching_layer <- function(object, ..., recompile. = FALSE) {
  caching_enabled <- any_is_substring_of(object$resource$name,
    object$resource$director$cached_resources())
  caching_enabled <- caching_enabled && !isTRUE(recompile.)

  if (!caching_enabled) {
    yield()
  } else {
    is_cached <- base::exists("caching_layer.value", envir = object$state)

    ## If this resource has been parsed before but any of its dependencies
    ## have been modified, we should wipe the cache.
    if (is_cached && isTRUE(object$injects$any_dependencies_modified)) {
      base::rm("caching_layer.value", envir = object$state)
      is_cached <- FALSE
    }

    if (is_cached) {
      object$state$caching_layer.value
    } else {
      value <- yield()
      object$state$caching_layer.value <- value
      value
    }
  }
}

# Apply the preprocessor to a resource. If parse. = TRUE, the parser will be
# applied as well.
preprocessor <- function(object, ..., parse. = TRUE) {
  director <- object$resource$director

  route <- director$match_preprocessor(object$resource$name)

  if (isTRUE(object$injects$virtual)) {
    filename <- NULL
  } else {
    filename <- object$state$filename <-
      director$filename(object$resource$name, absolute = TRUE)
  }

  object$injects %<<% list(
    # TODO: (RK) Use alist so these aren't evaluated right away.
    root = director$root,
    # TODO: (RK) Use find_director helper to go off root + project_name
    resource = function(...) director$resource(...),
    resource_name = object$resource$name,
    resource_exists = function(...) director$exists(...),
    helper = NULL # TODO: (RK) Allow helper parsing.
  )

  if (is.null(route)) {
    if (isTRUE(object$injects$virtual)) {
      stop("Cannot preprocess virtual resource without a preprocessor")
    }

    # No preprocessor for this resource.
    # Use the default preprocessor, base::source.
    default_preprocessor <- function(filename) {
      # TODO: (RK) Figure out correct environment assignment.
      base::source(filename, local = source_env)$value
    }
    source_env <- new.env(parent = parent.env(topenv(parent.env(environment())))) %<<%
      object$injects
    object$state$preprocessor.source_env <- source_env
    environment(default_preprocessor) <- 
      new.env(parent = object$resource$defining_environment) %<<%
      list(source_env = source_env)

    object$preprocessed <- list(
      value = default_preprocessor(filename),
      preprocessor_output = new.env(parent = emptyenv())
    )
  } else {

    object$state$preprocessor.source_env <- new.env(parent = object$injects)

    preprocessor_output <- new.env(parent = emptyenv())
    fn <- director$preprocessor(route)
    environment(fn) <- new.env(parent = environment(fn)) %<<% object$injects %<<% list(
      # TODO: (RK) Intersect with preprocessor formals.
      # TODO: (RK) Use alist so these aren't evaluated right away.
       resource = object$resource$name,
       director = director,
       filename = filename,
       args = list(...),
       source_env = object$state$preprocessor.source_env,
       source = function() eval.parent(quote(base::source(filename, source_env)$value)),
       preprocessor_output = preprocessor_output,
       "%||%" = function(x, y) if (is.null(x)) y else x
    )

    object$preprocessed <- list(
      value = fn(),
      preprocessor_output = preprocessor_output
    )
  }

  if (isTRUE(parse.)) {
    yield() # Apply parser.
  } else {
    object$preprocessed$value
  }
}

# Apply the parser to a resource. If parse. = TRUE, the parser will be
# applied as well.
parser <- function(object, ...) {
  director <- object$resource$director

  route <- director$match_parser(object$resource$name)

  if (is.null(route)) {
    # No parser for this resource.
    # Use the default parser, just grab the value.
    object <- object$preprocessed$value
  } else {
    fn <- director$parser(route)
    environment(fn) <- new.env(parent = environment(fn)) %<<% list(
      # TODO: (RK) Intersect with parser formals.
      # TODO: (RK) Use alist so these aren't evaluated right away.
       resource = object$resource$name,
       input = object$state$preprocessor.source_env,
       output = object$preprocessed$value,
       director = director,
       preprocessor_output = object$preprocessed$preprocessor_output,
       filename = object$state$filename,
       args = list(...),
       "%||%" = function(x, y) if (is.null(x)) y else x
    )
    object <- fn()
  }

  yield()
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
