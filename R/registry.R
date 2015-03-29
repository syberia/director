#' A persistent on-disk cache of R objects associated with a directory.
#'
#' Having a registry attached to a project is very helpful for maintaining
#' state across R sessions without encoding everything in the unreliable
#' \code{.RData} file.
#'
#' To create a registry, simply write \code{r <- registry("some/directory")}.
#' You can then use \code{r$set('some/key', some_value)} and
#' \code{r$get('some/key')} to set and retrieve values (arbitrary R objects).
#'
#' @docType class
#' @name registry
#' @rdname registry
#' @export
#' @exportClass registry
#' @examples
#' \dontrun{
#'   r <- registry('some/dir') # Create "some/dir" and make a registry there.
#'   r$set('some/key', value <- list(1,2,3))
#'   stopifnot(r$get('some/key'), value)
#' }
registry <- setRefClass('registry',
  fields = list(.root = 'character'),
  methods = list(
    #' Initialize a registry. A registry is responsible for maintaining
    #' an on-disk cache of R objects (configuration, temporary storage,
    #' associated with a directory).
    #'
    #' @param root character. The root of the registry. If it does not exist,
    #'    it (and any not yet existent parent directories) will be created.
    #' @param 
    #' @examples
    #' registry(dirname(tempfile()))
    initialize = function(root = NULL) {
      ## [A reference class docstring](http://stackoverflow.com/a/5931576/2540303)
      "Initialize a registry."
      
      # Reference class objects are sometimes initialized on package install, but
      # no arguments are passed! We let it through to avoid installation problems.
      if (is.null(root)) return(NULL) 

      enforce_type(root, "character", "registry$new")
      stopifnot(length(root) == 1)

      if (!file.exists(root)) {
        dir.create(root, showWarnings = FALSE, recursive = TRUE)
      }
      
      if (!file.info(root)$isdir) {
        stop("A registry's root must be a directory, not a file (you provided ",
             crayon::red(root), ")")
      }
      
      .root <<- normalizePath(root)
    },
    
    #' Retrieve an object from the registry.
    #'
    #' The key used to locate the object will be the directory/file 
    #' structure in the registry's root.
    #'
    #' @param key character. The path relative to the registry's root.
    #' @param ... additional keys which will be joined together with
    #'    code \code{base::file.path}. Thus, if you \code{get('a','b')},
    #'    you are asking for key \code{'a/b'}.
    #' @param soft logical. Whether or not to error if the registry key
    #'    requested does not exist. If \code{soft = TRUE} and the latter
    #'    condition holds, \code{NULL} will be returned instead. The
    #'    default is \code{soft = TRUE}.
    #' @return an R object stored in the registry under the given \code{key}.
    #'    This will be serialized as an RDS file relative to the root of
    #'    the registry. If \code{soft = TRUE}, \code{NULL} may be returned
    #'    if the \code{key} does not point to a valid registry key.
    #' @examples
    #' \dontrun{
    #'   r <- registry('some/dir')
    #'   r$get('foo') # gets key "foo"
    #'   r$get('foo', 'bar', 'baz') # get key "foo/bar/baz"
    #' }
    get = function(key, ..., soft = TRUE) {
      enforce_type(key, "character", "registry$get")

      if (length(rest <- c(...)) != 0) {
        key <- do.call('file.path', as.list(c(key, rest)))
      }

      key <- .sanitize_key(key, read = TRUE, soft = soft)
      if (!is.null(key)) (readRDS(key)) # do not use default invisibility
    },

    #' Place an object in the registry.
    #'
    #' The key used to locate the object will be the directory/file 
    #' structure in the registry's root. The object is serialized using
    #' \code{saveRDS}. 
    #'
    #' @param key character. The path relative to the registry's root.
    #' @param value ANY. Some R object to serialize into the registry.
    #' @examples
    #' registry(dirname(tempfile))$set('example/key', 'example_value')
    #' # The directory 'example' was created under the registry's root
    #' # with a filename 'key' that holds the string 'example_value'.
    set = function(key, value) {
      # TODO: (RK) Warn on overwrite?

      key <- .sanitize_key(key, read = FALSE)
      error_handler <- function(e) {
        stop('Failed to save registry key ', sQuote(crayon::red(key)), 
             ' in registry with root ', sQuote(crayon::blue(.root)), 
             " because: \n\n", crayon::yellow(e$message), "\n\n")
      }
      tryCatch(error = error_handler, saveRDS(value, key))
    },

    #' Sanitize a registry key to ensure it can point to a filename.
    #' 
    #' @param key character. The registry key to sanitize. Note that
    #'    this will determine an actual file structure, so if the
    #'    the key \code{'foo/bar'} is used, an actual directory
    #'    \code{'foo'} will be created inside the registry's root.
    #' @param read logical. Whether a read or write operation is being
    #'    performed on the registry. In the former scenario, this method
    #'    tests that an associated filename exists. In case of a write 
    #'    operation, the requisite directories are created. Thus,
    #'    setting \code{"nonexistent/file"} will error if \code{read = TRUE},
    #'    but create the \code{"nonexistent"} directory if \code{tread = FALSE}.
    #'    The default is \code{read = TRUE}.
    #' @param soft logical. Whether or not to error if \code{read = TRUE} and
    #'    the filename implied by \code{key} does not exist. If you try to
    #'    create a key one of whose parent directories is actually a file
    #'    while \code{read = FALSE}, it will still error however. For example,
    #'    sanitizing key \code{"foo/bar/baz"} when \code{"foo/bar"} is a file
    #'    with \code{read = FALSE} results in an error. The default is
    #'    \code{TRUE}.
    #' @examples
    #' \dontrun{
    #'   r <- registry(dirname(tempfile()))
    #'   r$.sanitize_key('nonexistent/file') # This will complain
    #'   r$.sanitize_key('nonexistent/file', read = FALSE)
    #'   # This will create the `nonexistent` directory in the registry root.
    #' }
    .sanitize_key = function(key, read = TRUE, soft = TRUE) {
      stopifnot(is.character(key))

      if (length(key) == 0) return(character(0))
      if (length(key) > 1) return(vapply(key, .self$.sanitize_key, character(1)))

      # Prevent security shenanigans.
      if (grepl('..', key, fixed = TRUE))
        stop('Registry keys cannot contain two consecutive dots (the ',
             'key ', sQuote(crayon::red(key)), ' was given in ',
             'registry with root ', sQuote(crayon::blue(.root)))

      if (isTRUE(read)) {
        if (!file.exists(filename <- file.path(.root, key))) {
          if (soft) NULL
          else stop('There is no registry item with key ',
                    sQuote(crayon::red(key)), ' in registry with root ',
                    sQuote(crayon::blue(.root)))
        } else if (file.info(filename)$isdir) {
          stop('There is no registry item with key ', sQuote(crayon::red(key)),
               ' in registry with root ', sQuote(crayon::blue(.root)),
               ' because this key points to a directory.')
        } else filename
      } else {
        warning_handler <- function(e) {
          if (!is(e, 'warning') || grepl("reason 'Not a directory'", e$message, fixed = TRUE))
            stop('Cannot create registry key ', sQuote(crayon::red(key)),
                 ' in registry with root ', sQuote(crayon::blue(.root)),
                 " because: \n\n", crayon::yellow(e$message), "\n\n")
        }
        if ((dir <- dirname(key)) != '.') { # Not root level, has parent dir
          if (file.exists(d <- file.path(.root, dir)) && !file.info(d)$isdir)
            warning_handler(list(message =
              paste(sQuote(d), 'is a file but must be a directory')))
          tryCatch(warning = warning_handler, dir.create(file.path(.root, dir),
            recursive = TRUE))
        }
        file.path(.root, key)
      }
    } # end .sanitize_key method
  )
)

#' @docType function
#' @name registry
#' @export
NULL
