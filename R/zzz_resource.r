setClassUnion('listOrNULL', c('list', 'NULL'))

#' Representation of a director resource.
#'
#' @docType refClass
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
    
    value = function() {
      compile()
      .value
    },

    # Compile a resource using a resource handler.
    #
    # @param tracking logical. Whether or not to perform modification tracking
    #   by pushing accessed resources to the director's stack. The default is
    #   \code{TRUE}.
    compile = function(tracking = TRUE) {
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

      # TODO: (RK) Avoid this awful duplication!!
      resource_cache_key <- file.path('resource_cache', digest(resource_key))
       
      # TODO: (RK) Better resource provision injection
      source_args$local$resource <<- function(...) director$resource(...)

      value <- do.call(base::source, source_args)$value
      
      # Cache dependencies.
      dependencies <- 
        Filter(function(dependency) dependency$level == local_nesting_level, 
               director$.stack$peek(TRUE))
      if (any(vapply(dependencies, getElement, logical(1), name = 'modified')))
        modified <<- TRUE

      .dependencies <<- vapply(dependencies, getElement, character(1), name = 'key')
      director$.cache[[resource_cache_key]]$dependencies <<- .dependencies 
      director$.cache[[resource_cache_key]]$modified <<- modified

      while (!director$.stack$empty() && director$.stack$peek()$level == local_nesting_level)
        director$.stack$pop()

      .value    <<- parse(value, source_args$local)
      .compiled <<- TRUE
    },

    # Parse a resource after it has been sourced.
    # 
    # @param value ANY. The return value of the resource file.
    # @param provides environment. The local environment it was sourced in.
    # @param the parsed object.
    parse = function(value, provides) {
      # TODO: (RK) Resource parsers?
      list(value, provides)
    },

    show = function() {
      cat("Resource", sQuote(resource_key), "under director: \n")
      director$show()
    }

  )
)


#' @docType function
#' @name director
#' @export
NULL
