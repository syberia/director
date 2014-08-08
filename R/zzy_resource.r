setClassUnion('listOrNULL', c('list', 'NULL'))

#' Representation of a director resource.
#'
#' @docType class
#' @name directorResource
#' @rdname directorResource
directorResource <- setRefClass('directorResource',
  fields = list(current = 'listOrNULL', cached = 'listOrNULL',
                modified = 'logical', resource_key = 'character',
                source_args = 'list', director = 'director',
                .dependencies = 'character', .compiled = 'logical',
                .value = 'ANY'),
  methods = list(
    initialize = function(current, cached, value, modified, resource_key,
                          source_args, director) {
      current      <<- current
      cached       <<- cached
      modified     <<- modified
      resource_key <<- resource_key
      source_args  <<- source_args
      director     <<- director
      .compiled    <<- FALSE
    },
    
    value = function(...) {
      compile(...)
      .value
    },

    # Compile a resource using a resource handler.
    #
    # @param tracking logical. Whether or not to perform modification tracking
    #   by pushing accessed resources to the director's stack. The default is
    #   \code{TRUE}.
    compile = function(..., tracking = TRUE) {
      if (isTRUE(.compiled)) return(TRUE) 

      if (!is.element('local', names(source_args)))
        stop("To compile ", sQuote(source_args[[1]]), " you must include ",
             dQuote('local'), " in the list of arguments to pass to base::source")
      else if (!is.environment(source_args$local))
        stop("To compile ", sQuote(source_args[[1]]), " you must include an ",
             "environment in the ", dQuote('local'), " parameter to base::source.")

      # We will be tracking what dependencies (other resources) are loaded
      # during the compilation of this resource. We have a dependency nesting
      # level on the director object that counts how deep we are within 
      # resource compilation (i.e., if a resource needs another resource
      # which needs another resources, etc.).
      if (director$.dependency_nesting_level == 0) director$.stack$clear()
      director$.dependency_nesting_level <<- director$.dependency_nesting_level + 1L
      on.exit(director$.dependency_nesting_level <<- director$.dependency_nesting_level - 1L)
      local_nesting_level <- director$.dependency_nesting_level 
 
      # TODO: (RK) Better resource provision injection
      source_args$local$resource <<- function(...) director$resource(...)$value()

      value <- do.call(base::source, source_args)$value
      
      .value <<- parse(value, source_args$local, list(...))

      # Cache dependencies.
      dependencies <- 
        Filter(function(dependency) dependency$level == local_nesting_level, 
               director$.stack$peek(TRUE))
      if (any(vapply(dependencies, function(d) d$resource$modified, logical(1))))
        modified <<- TRUE

      cached$dependencies <<- vapply(dependencies, getElement, character(1), name = 'key')
      cached$modified     <<- modified
      update_cache()

      while (!director$.stack$empty() && director$.stack$peek()$level == local_nesting_level)
        director$.stack$pop()

      .compiled <<- TRUE
    },
    recompile = function(...) { 
      .compiled <<- FALSE
      compile(...)
    },

    # Parse a resource after it has been sourced.
    # 
    # @param value ANY. The return value of the resource file.
    # @param provides environment. The local environment it was sourced in.
    # @param the parsed object.
    parse = function(value, provides, args = list()) {
      # TODO: (RK) Resource parsers?
      route <- Find(function(x) substring(resource_key, 1, nchar(x)) == x,
                    names(director$.parsers))
      if (is.null(route)) value
      else {
        fn <- director$.parsers[[route]]
        env <- new.env(parent = environment(fn))
        environment(fn) <- env # TODO: (RK) Test this!
        environment(fn)$resource <- resource_key
        environment(fn)$input    <- provides
        environment(fn)$output   <- value
        environment(fn)$director <- director
        environment(fn)$resource_body <- current$body
        environment(fn)$modified <- modified
        environment(fn)$resource_object <- .self
        environment(fn)$args     <- args
        
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
      director$.cache[[cache_key]]$dependencies <<- cached$dependencies
      director$.cache[[cache_key]]$modified     <<- cached$modified
    },

    dependencies = function() {
      get_dependencies <- function(key) {
        deps <- director$.cache[[resource_cache_key(key)]]$dependencies %||% character(0)
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
    }

  )
)


#' @docType function
#' @name director
#' @export
NULL
