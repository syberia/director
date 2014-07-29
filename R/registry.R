#' A persistent on-disk cache of R objects associated with a directory.
#'
#' @docType class
#' @name registry
#' @rdname registry
#' @export
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
      if (is.null(root)) return(NULL) # Empty object
      if (!is.character(root))
        stop("A registry must be initialized with a character path to a root ",
             "directory")

      if (!file.exists(root))
        dir.create(root, showWarnings = FALSE, recusrive = TRUE)
      
      if (!file.info(root)$isdir)
        stop("A registry's root must be a directory, not a file (you provided ",
             colourise(root, 'red'), ")")
      
      .root <<- normalizePath(root)
    },
    
    #' Retrieve an object from the registry.
    #'
    #' The key used to locate the object will be the directory/file 
    #' structure in the registry's root.
    #'
    #' @param key character. The path relative to the registry's root.
    #' @param soft logical. Whether or not to error if the registry key
    #'    requested does not exist. If \code{soft = TRUE} and the latter
    #'    condition holds, \code{NULL} will be returned instead. The
    #'    default is \code{soft = TRUE}.
    #' @return an R object stored in the registry under the given \code{key}.
    #'    This will be serialized as an RDS file relative to the root of
    #'    the registry. If \code{soft = TRUE}, \code{NULL} may be returned
    #'    if the \code{key} does not point to a valid registry key.
    get = function(key, soft = TRUE) {
      key <- .sanitize_key(key, read = TRUE, soft = soft)
      if (is.null(key)) NULL else (readRDS(key)) # do not use default invisibility
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
      key <- .sanitize_key(key, read = FALSE)
      error_handler <- function(e) {
        stop('Failed to save registry key ', sQuote(colourise(key, 'red')), 
             ' in registry with root ', sQuote(colourise(.root, 'blue')), 
             " because: \n\n", colourise(e$message, 'yellow'), "\n\n")
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
             'key ', sQuote(colourise(key, 'red')), ' was given in ',
             'registry with root ', sQuote(colourise(.root, 'blue')))

      if (isTRUE(read)) {
        if (!file.exists(filename <- file.path(.root, key))) {
          if (soft) NULL
          else stop('There is no registry item with key ',
                    sQuote(colourise(key, 'red')), ' in registry with root ',
                    sQuote(colourise(.root, 'blue')))
        } else if (file.info(filename)$isdir) {
          stop('There is no registry item with key ', sQuote(colourise(key, 'red')),
               ' in registry with root ', sQuote(colourise(.root, 'blue')),
               ' because this key points to a directory.')
        } else filename
      } else {
        warning_handler <- function(e) {
          if (grepl("reason 'Not a directory'", e$message))
            stop('Cannot create registry key ', sQuote(colourise(key, 'red')),
                 ' in registry with root ', sQuote(colourise(root, 'blue')),
                 " because: \n\n", colourise(e$message, 'yellow'), "\n\n")
        }
        if ((dir <- dirname(key)) != '.') { # Not root level, has parent dir
          tryCatch(warning = warning_handler, dir.create(file.path(.root, dir),
            showWarnings = FALSE, recursive = TRUE))
        }
        file.path(.root, key)
      }
    } # end .sanitize_key method
  )
)

