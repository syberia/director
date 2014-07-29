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
      if (!file.exists(root)) dir.create(root, FALSE, TRUE)
      
      if (!file.info(root)$isdir)
        stop("A registry's root must be a directory, not a file (you provided ",
             colourise(root, 'red'), ")")
      
      .root <<- root
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
      key <- .sanitize_key(key)      
      error <- function(e) {
        stop('Failed to save registry key ', sQuote(colourise(red)), 
             ' in registry with root ', sQuote(colourise(blue)), 
             ' because: ', e$message)
      }
      tryCatch(error = error, saveRDS(value, key))
    }

    
  )
)
#
#.get_registry_key <- function(key, registry_dir, soft = TRUE) {
#  filename <- .sanitize_registry_key(key, registry_dir, soft = soft)
#  if (is.null(filename)) NULL else (readRDS(filename)) # do not use default invisibility
#}
#
#.set_registry_key <- function(key, value, registry_dir) {
#  filename <- .sanitize_registry_key(key, registry_dir, read = FALSE)
#  tryCatch(saveRDS(value, filename), error = function(e)
#           stop('Failed to save Syberia registry key "', key, "' because: ", e$message))
#  key
#}
#
#.sanitize_registry_key <- function(key, registry_dir, read = TRUE, soft = FALSE) {
#  if (grepl('..', key, fixed = TRUE))
#    stop('Syberia registry keys cannot contain two consecutive dots')
#
#  if (read) {
#    if (!file.exists(filename <- file.path(registry_dir, key))) {
#      if (soft) NULL
#      else stop('There is no Syberia registry item with key "', key, '"')
#    } else if (file.info(filename)$isdir)
#      stop('There is no Syberia registry item with "', key, '", ',
#           'because this key points to a directory.')
#    else filename
#  } else {
#    if ((dir <- dirname(key)) != '.') {
#      tryCatch(dir.create(file.path(registry_dir, dir), recursive = TRUE),
#               warning = handler <- function(e) {
#                 if (grepl("reason 'Not a directory'", e$message))
#                   stop('Cannot create Syberia registry key "', key, '"')
#               })
#    }
#    file.path(registry_dir, key)
#  }
#}
#
#
