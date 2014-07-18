# All functions related to traversal of file system for grabbing Syberia related files
#
# By convention, the structure from a syberia root project will look like this:
# 
# - data     # Data preparation for data sources coming from an external API
#   - sources
#     - data_source1
#       - data_source1.r
#       - helpers.r
#     - data_source2.r 
#   - test   # Unit tests for data preparation for data sources
# - models   # Files that parametrize the modeling process for each model version
#   - dev    # The development environment to be used as a sandbox
#   - prod   # Models will be copied over from dev to prod for deployment
#   - shared # Shared helpers for all models
#   - test   # Unit tests for the models - should be environment-agnostic (dev or prod)
# - bin      # Production scripts
# - syberia.config  # Configuration file for syberia project

#' Determine the root path of the current Syberia project.
#'
#' The root of the Syberia project is considered to be the directory containing
#' the \code{syberia.config} file.
#'
#' @name syberia_root
#' @param filename character. If specified, it will attempt to find the Syberia root
#'   relative to the file name by traversing up its parent directories.
#'   If not given, Syberia will try to intelligently discern the current
#'   Syberia project by first looking at the cache for the previously used
#'   Syberia project and then by looking at the current directory. If no
#'   project is found, the last resort is to look for a \code{syberia.root}
#'   option. If not found, this will return \code{NULL}.
#' @param error logical. If \code{TRUE}, it will return an error if the path
#'   is not found.
#' @export
#' @return see the \code{filename} parameter
syberia_root <- function(filename = NULL, error = FALSE) {
  if (missing(filename)) {
    # If no filename was given, see if a syberia configuration was
    # given previously.
    return(
      get_cache('syberia_project') %||%
      syberia_root(getwd(), error = FALSE) %||%
      options('syberia.root')[[1]]
    )
  }
  
  original_filename <- filename
  # TODO: Windows support?
  if (!'/' %in% strsplit(filename, '')[[1]]) filename <- file.path('.', filename)
  filename <- suppressWarnings(normalizePath(filename))
  fileinfo <- file.info(filename)
  if (is.na(fileinfo$isdir) || !fileinfo$isdir) filename <- dirname(filename)

  repeat {
    if (file.exists(file.path(filename, 'syberia.config'))) break
    prev_dir <- filename
    filename <- suppressWarnings(normalizePath(dirname(filename)))
    if (filename == prev_dir)
      # Reached root of filesystem
      if (error)
        stop("No syberia project found relative to: ",
           original_filename, call. = FALSE)
      else return(NULL)
  }
  set_cache(filename, 'syberia_project')
  filename
}

# TODO(RK): A syberia_set<- replace method that can set, e.g., the root explicitly

#' @export
syberia_project <- syberia_root

#' Verifies that the given filename points to the base of a syberia project.
#'
#' @param filename character. The directory supposedly containing a syberia
#'   project.
#' @return TRUE or FALSE according as the directory is or is not a Syberia
#'   project (including if the directory does not exist).
#' @name is.syberia_project
is.syberia_project <- function(filename) {
  if (!is.character(filename)) FALSE
  else if (!tryCatch(file.exists(filename))) FALSE
  else syberia_root(filename) == normalizePath(filename)
}


#' Find all the model objects in a Syberia project.
#'
#' The convention is that model files have the same name
#' as the directory they are contained in (this allows other
#' files in the same directory to be used as helper files).
#' If no such file exists in a directory, all files are
#' assumed to be model files. For example, if we have
#'
#' default/en-US/model1.r
#' default/en-US/model2/model2.r
#' default/en-US/model2/helper.r
#'
#' only the first two will be considered to be model objects.
#' If there was a default/en-US/en-US.r, then the first would
#' no longer be considered a model object (it would be considered
#' a file with helper functions).
#'
#' @param pattern character. A set of characters by which to filter.
#'   This uses the same format as the popular ctrl+p plugin for vim.
#'   Namely, it will look for adjacent instances of such characters
#'   regardless of any interpolating characters. For example,
#'   'ace' will match 'abcde' but also 'abcdfghe' but not 'aebcd'.
#' @param env character. The syberia environment (e.g., \code{'dev'} or
#'   \code{'prod'}. The default is \code{c('dev', 'prod')}.
#' @param root character. The root of the syberia project. The default
#'   is \code{syberia_root()}.
#' @param by_mtime logical. Whether or not to sort the models in descending
#'   order by last modified time. The default is \code{TRUE}.
#' @param fixed logical. Whether or not to use smart interpolation, like in
#'   the description for the \code{pattern} argument. If \code{TRUE},
#'   only substring matching is used.
#' @seealso \code{\link{syberia_root}}
#' @export
#' @return a list of filenames containing syberia models
syberia_models <- function(pattern = '', env = c('dev', 'prod'),
                           root = syberia_root(), by_mtime = TRUE, fixed = FALSE) {
  for (env_name in env)
    if (tolower(substring(pattern, 1, nchar(env_name))) == tolower(env_name)) {
      # If the pattern has "dev" or "prod" in the beginning, look only
      # in that environment.
      env <- env_name
      pattern <- substring(pattern, nchar(env_name) + 2, nchar(pattern))
      break 
    } 
  unlist(lapply(env, function(env_name) file.path(env_name,
    syberia_objects(pattern, file.path(root, 'models', env_name), by_mtime, fixed))))
}

#' Find all the data sources in a Syberia project.
#'
#' The convention is that data source files have the same name
#' as the directory they are contained in (this allows other
#' files in the same directory to be used as helper files).
#' If no such file exists in a directory, all files are
#' assumed to be data source files. For example, if we have
#'
#' some_data_type/data_source1.r
#' some_data_type/data_source2/data_source2.r
#' some_data_type/data_source2/helpers.r
#'
#' only the first two will be considered to be data sources.
#' If there was a some_data_type/some_data_type.r, then the first would
#' no longer be considered a model object (it would be considered
#' a file with helper functions).
#'
#' @param pattern character. A set of characters by which to filter.
#'   This uses the same format as the popular ctrl+p plugin for vim.
#'   Namely, it will look for adjacent instances of such characters
#'   regardless of any interpolating characters. For example,
#'   'ace' will match 'abcde' but also 'abcdfghe' but not 'aebcd'.
#' @param type character. This can be either \code{"sources"} or \code{"test"}.
#'   The default is the former, whereas the latter will fetch data source tests.
#' @param root character. The root of the syberia project. The default
#'   is \code{syberia_root()}.
#' @param by_mtime logical. Whether or not to sort the data sources in descending
#'   order by last modified time. The default is \code{TRUE}.
#' @param fixed logical. Whether or not to use smart interpolation, like in
#'   the description for the \code{pattern} argument. If \code{TRUE},
#'   only substring matching is used.
#' @seealso \code{\link{syberia_root}}
#' @export
#' @return a list of filenames containing syberia data sources
syberia_data_sources <- function(pattern = '', type = "sources", root = syberia_root(),
                                 by_mtime = TRUE, fixed = FALSE) {
  stopifnot(type %in% c('sources', 'test'))
  gsub(syberia_objects(pattern, file.path(root, 'data', type), by_mtime, fixed))
}

#' Find all the syberia objects of the given type and subtype in a Syberia project.
#'
#' Syberia objects can refer to models, data sources, or tests. The essence
#' of the idea is that the \code{pattern} parameter specifies a set of consecutive
#' character by which to look for, and \code{type} specifies the subdirectory
#' (an additional subdirectory of that directory can be set using the \code{subtype}
#' parameter).
#'
#' For example, if we are looking for models in the prod environment matching "gbm",
#' we could try: \code{syberia_objects('gbm', 'models', 'prod')}.
#'
#' Note, however, that the first argument (\code{pattern}) does not look for a
#' substring match, but an interpolated match: for example, looking for 'abc'
#' will match "a1b2c" or "model_a/submodel_bc" but will not match "acb" or
#' any string where the characters 'a', 'b', and 'c' do not appear consecutively
#' (with arbitrary strings in between them).
#'
#' @param pattern character. A set of characters by which to filter.
#'   This uses the same format as the popular ctrl+p plugin for vim.
#'   Namely, it will look for adjacent instances of such characters
#'   regardless of any interpolating characters. For example,
#'   'ace' will match 'abcde' but also 'abcdfghe' but not 'aebcd'.
#' @param base character. A subdirectory to look in. For example,
#'   \code{type = file.path(syberia_root(), 'models')} will look in the
#'   \code{models} subdirectory of the root directory of the currently
#'   active Syberia project, whereas the same with \code{'models/dev'}
#'   will look in the \code{models/dev} subdirectory. The default
#'   is \code{syberia_root()}.
#' @param by_mtime logical. Whether or not to sort the models in descending
#'   order by last modified time. The default is \code{TRUE}.
#' @param fixed logical. Whether or not to use smart interpolation, like in
#'   the description for the \code{pattern} argument. If \code{TRUE},
#'   only substring matching is used.
#' @seealso \code{\link{syberia_models}}
#' @export
#' @return a list of filenames containing syberia objects
#' @name syberia_objects
syberia_objects <- function(pattern = '', base = syberia_root(),
                            by_mtime = TRUE, fixed = FALSE) {
  stopifnot(length(base) == 1)
  fixed <- !identical(fixed, FALSE) # Ensure this parameter is logical.
  strip_extension <- function(x) gsub('\\.[rR]$', '', x)
  abs_dirname <- function(x) if ((tmp <- dirname(x)) == '.') base else tmp

  all_files <- list.files(base, recursive = TRUE)

  # Idempotent objects are those whose filename is the same as the
  # name of the directory they reside in. This is helpful for, e.g.,
  # helper functions.
  idempotent_objects <- grep("([^/]+)/\\1\\.[rR]$", all_files, value = TRUE)
  base_files <- all_files[!grepl("/", all_files, fixed = TRUE)] # TODO: (RK) Make OS-agnostic
  base_idempotent_objects <- 
    Filter(function(filename) strip_extension(filename) == basename(base), base_files)
  idempotent_objects <- c(idempotent_objects, base_idempotent_objects)
  idempotent_objects <- vapply(idempotent_objects, abs_dirname, character(1))

  # Find the files that belong in directories of idempotent objects --
  # that is, helper files, and exclude those from being processable
  # by this function completely.
  helper_functions <- vapply(all_files,
    function(file) is.element(abs_dirname(file), idempotent_objects), logical(1))
  all_files <- all_files[!helper_functions]

  # We now apply the filter to all files and the idempotent objects --
  # this separation is necessary to prevent things like looking for "2.1.2"
  # catching "model/2.1.1/2.1.1", which would be wrong.
  if (!identical(pattern, '')) {
    pattern <- strip_extension(pattern) # Strip file extension
    if (!fixed) {
      pattern <- gsub('([]./\\*+()])', '\\\\\\1', pattern)
      pattern <- gsub('([^\\])', '\\1.*', pattern) # turn this into ctrl+p
    }
    suppressWarnings({ # ignore.case = T with fixed = T gives harmless warning 
      all_files <- grep(pattern, all_files, fixed = fixed,
                        value = TRUE, ignore.case = TRUE)
      idempotent_objects <- idempotent_objects[
        grep(pattern, names(idempotent_objects), fixed = fixed, FALSE, ignore.case = TRUE)]
    })
  }

  # Finally, put the results together: we were looking for either
  # non-idempotent or idempotent objects passing the filter, being careful
  # to not use the whole path of the latter.
  all_files <- unname(c(all_files, names(idempotent_objects)))

  if (identical(by_mtime, TRUE)) {
    descending_by_modification_time <-
      -vapply(file.path(base, all_files),
              function(f) file.info(f)$mtime, numeric(1))
    all_files <- all_files[order(descending_by_modification_time)]
  }

  all_files
}

