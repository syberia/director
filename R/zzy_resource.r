setClassUnion('listOrNULL', c('list', 'NULL'))

#' Representation of a director resource.
#'
#' @docType class
#' @name directorResource
#' @rdname directorResource
directorResource <- setRefClass('directorResource',
  fields = list(current = 'listOrNULL', cached = 'listOrNULL',
                modified = 'logical', resource_key = 'character',
                source_args = 'list', director = 'ANY',
                defining_environment = 'environment',
                .dependencies = 'character', .compiled = 'logical',
                .value = 'ANY'),
  methods = list(
    initialize = function(current, cached, modified, resource_key,
                          source_args, director, defining_environment) {
      current      <<- current
      cached       <<- cached
      modified     <<- modified
      resource_key <<- resource_key
      source_args  <<- source_args
      director     <<- director
      defining_environment <<- defining_environment
      .compiled    <<- FALSE
    },
    
    value = function(..., recompile. = FALSE) {
      if (isTRUE(recompile.)) recompile(...)
      else if (is_cached() && !any_dependencies_modified()) .value <<- cached$value
      else compile(...)
      .value
    },

    # Compile a resource using a resource handler.
    #
    # @param parse. logical. Whether or not to apply parsers. Note that
    #   it is impossible to not apply preprocessors, since it is
    #   the preprocessor's responsibility to source the file of the resource.
    # @param tracking logical. Whether or not to perform modification tracking
    #   by pushing accessed resources to the director's stack. The default is
    #   \code{TRUE}.
    compile = function(..., parse. = TRUE, tracking = TRUE) {
      if (isTRUE(.compiled)) return(TRUE) 

      if (!is.element('local', names(source_args)))
        stop("To compile ", sQuote(source_args[[1]] %||% 'this resource'),
             " you must include ", dQuote('local'),
             " in the list of arguments to pass to base::source")
      else if (!is.environment(source_args$local))
        stop("To compile ", sQuote(source_args[[1]] %||% 'this resource'),
             " you must include an ", "environment in the ", dQuote('local'),
             " parameter to base::source.")

      # We will be tracking what dependencies (other resources) are loaded
      # during the compilation of this resource. We have a dependency nesting
      # level on the director object that counts how deep we are within 
      # resource compilation (i.e., if a resource needs another resource
      # which needs another resources, etc.).
      director$clear_resource_stack()
      director$increment_nesting_level()
      on.exit(director$decrement_nesting_level())
      local_nesting_level <- director$nesting_level()
 
      # TODO: (RK) Better resource provision injection
      if (!base::exists('..director_inject', envir = parent.env(source_args$local), inherits = FALSE)) {
        injects <- new.env(parent = parent.env(source_args$local))
        scoping_environment <- local({
          e <- new.env(parent = defining_environment)
          e$director <- director
          e
        })
        injects$..director_inject <- TRUE
        injects$root <- function(x, ...) director$root()
        injects$resource <- function(x, ...) director$resource(x)$value(...)
        environment(injects$resource) <- scoping_environment
        injects$resource_name <- resource_key
        injects$resource_exists <- function(...) director$exists(...)
        injects$helper   <-
          function(...) director$resource(..., helper = TRUE)$value(parse. = FALSE)
        environment(injects$helper) <- scoping_environment
        parent.env(source_args$local) <<- injects
      }

      value <- evaluate(source_args, list(...))
      if (isTRUE(parse.)) .value <<- parse(value, source_args$local, list(...))
      else .value <<- value$value
      cache_value_if_necessary()

      # Cache dependencies.
      dependencies <- 
        Filter(function(dependency) dependency$level == local_nesting_level, 
               director$dependency_stack$peek(TRUE))
      if (any(vapply(dependencies, function(d) d$resource$modified, logical(1))))
        modified <<- TRUE

      cached$dependencies <<- vapply(dependencies, getElement, character(1), name = 'key')
      cached$modified     <<- modified
      update_cache()

      while (!director$dependency_stack$empty() && director$dependency_stack$peek()$level == local_nesting_level)
        director$dependency_stack$pop()

      .compiled <<- TRUE
    },
    recompile = function(...) { 
      .compiled <<- FALSE
      compile(...)
    },

    # Evaluate a resource's R file.
    # 
    # This is a straightforward call to \code{base::source}, although if a 
    # preprocessor was registered, this will be executed before the file is sourced.
    #
    # A preprocessor function has the same available locals as a parser,
    # although it also has an environment \code{preprocessor_output},
    # and the \code{source_args} that are meant to be passed to \code{base::source}.
    #
    # This is an environment in which the preprocessor
    # may place computations, which will be available in the parser via
    # the \code{preprocessor_output} provider. The return value of the
    # preprocessor will be the final resource vlaue (so a preprocessor must
    # call \code{base::source} manually).
    #
    # Preprocessors are useful for doing things like (1) parsing through a
    # resource's source code to extract documentation, and (2) injecting
    # information into the local environment prior to sourcing a resource.
    #
    # Note: If \code{base::source} is called in the preprocessor without
    # \code{local = source_args$local}, the parser will not be able to access
    # the \code{input} that was generated during sourcing.
    # 
    # TODO: (RK) Provide examples.
    #
    # @param source_args list. The parameters to pass to \code{base::source}
    #   when the file is evaluated.
    # @param args list. Any additional arguments passed when calling \code{value()}.
    # @return a list with \code{value} and \code{preprocessor_output},
    #   the former the result of the preprocessor application, and the latter
    #   the environment that is made available to the parser later on.
    evaluate = function(source_args, args = list()) {
      route <- director$match_preprocessor(resource_key)

      if (is.null(route)) {
        fn <- function(source_args) { do.call(base::source, source_args)$value }
        environment(fn) <- defining_environment
        list(value = fn(source_args), preprocessor_output = emptyenv())
      }
      else {
        fn <- director$preprocessor(route)
        env <- new.env(parent = environment(fn))
        environment(fn) <- env # TODO: (RK) Test this!
        environment(fn)$resource        <- resource_key
        environment(fn)$director        <- director
        environment(fn)$resource_body   <- current$body
        environment(fn)$modified        <- modified
        environment(fn)$resource_object <- .self
        environment(fn)$source_args     <- source_args
        environment(fn)$args            <- args
        environment(fn)$source  <-
          function() eval.parent(quote(do.call(base::source, source_args)$value))
        environment(fn)$preprocessor_output <-
          preprocessor_output <- new.env(parent = emptyenv())
        assign("%||%", function(x, y) if (is.null(x)) y else x, envir = environment(fn))
        list(value = fn(), preprocessor_output = preprocessor_output)
      }
    },

    # Parse a resource after it has been sourced.
    # 
    # @param value ANY. The return value of the resource file.
    # @param provides environment. The local environment it was sourced in.
    # @param args list. Any additional arguments passed when calling \code{value()}.
    # @param the parsed object.
    parse = function(value, provides, args = list()) {
      # TODO: (RK) Resource parsers?
      route <- director$match_parser(resource_key)
      if (is.null(route)) value$value
      else {
        fn <- director$parser(route)
        env <- new.env(parent = environment(fn))
        environment(fn) <- env # TODO: (RK) Test this!
        environment(fn)$resource            <- resource_key
        environment(fn)$input               <- provides
        environment(fn)$output              <- value$value
        environment(fn)$preprocessor_output <- value$preprocessor_output
        environment(fn)$director            <- director
        environment(fn)$resource_body       <- current$body
        environment(fn)$modified            <- modified
        environment(fn)$resource_object     <- .self
        environment(fn)$args                <- args
        
        assign("%||%", function(x, y) if (is.null(x)) y else x, envir = environment(fn))
        fn()
      }
    },

    show = function() {
      cat("Resource", sQuote(resource_key), "under director: \n")
      director$show()
    },

    update_cache = function() {
      cache_key <- resource_cache_key(resource_key)
      cache              <- director$cache$get(cache_key)
      cache$dependencies <- cached$dependencies
      cache$modified     <- cached$modified
      director$cache$set(cache_key, cache)
    },

    dependencies = function() {
      get_dependencies <- function(key) {
        deps <- director$cache$get(resource_cache_key(key))$dependencies %||% character(0)
        as.character(c(deps, sapply(deps, get_dependencies), recursive = TRUE))
      }
      unique(c(recursive = TRUE, as.character(cached$dependencies),
        sapply(cached$dependencies, get_dependencies)))
    },

    # TODO: (RK) Test this method!
    dependencies_modified = function() {
      dependency_resources <- lapply(dependencies(), director$resource, soft = TRUE)
      # TODO: (RK) Do we need to worry about helpers v.s. non-helpers?

      those_modified <- vapply(dependency_resources,
        function(r) r$any_dependencies_modified(), logical(1))

      vapply(dependency_resources[those_modified],
             function(r) r$resource_key, character(1))
    },

    # TODO: (RK) Test this method!
    any_dependencies_modified = function() {
      modified || length(dependencies_modified()) > 0
    },

    cache_value_if_necessary = function() {
      if (!caching_enabled()) return()
      if (is(.value, 'uninitializedField')) {
        stop("directorResource$cache_value_if_necessary: Cannot cache resource ",
             "value because it has not been parsed.")
      }
      # We need to use `[` and not `$` or NULLs won't be cached.
      cached['value'] <<- list(value = .value)
      cache_key   <- resource_cache_key(resource_key)
      cache_entry <- director$cache$get(cache_key)
      cache_entry['value'] <- list(value = .value)
      director$cache$set(cache_key, cache_entry)
    },

    caching_enabled = function() {
      any_is_substring_of(resource_key, director$cached_resources())
    },
    is_cached = function() { is.element('value', names(cached)) }

  )
)


#' @docType function
#' @name director
#' @export
NULL
