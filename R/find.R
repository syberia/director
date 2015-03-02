#' Find resources within a director project.
#'
#' @note
#' The available search methods are:
#'
#' \itemize{
#'   \item{wildcard}{Similar to Sublime or vim's ctrl + P, this method
#'     of search will look for consecutive appearances of characters.
#'     For example, if we have a resource \code{"some_resource"}, then
#'     looking for \code{"so"}, \code{"sre"} or even \code{"smsrc"} will
#'     return a match, since those characters occur consecutively in the
#'     original resource name.}
#'   \item{partial}{This method will try to find a substring that
#'     matches the resource name. For example, if we have
#'     \code{"dir/some_resource"}, then looking for \code{"dir/some"} will
#'     return a match.}
#'   \item{exact}{The exact name of the resource. In this mode, either a 
#'     single string (the resource name itself) or \code{character(0)} will
#'     be returned.}
#' }
#'
#' @param search character. The resources to search for. The default is
#'    \code{""}, which will list all resources within the \code{base}.
#' @param method character. The search method. The available options
#'    are \code{"wildcard"}, code{"substring"}, or \code{"exact"}. See the function
#'    description for the full explanation of these methods. The default is
#'    \code{"wildcard"}.
#' @param base character. A prefix under which to look for. For example,
#'    if \code{base = "subdir"}, then only resources under the \code{"subdir"}
#'    directory (relative to the director root) will be returned. The default is
#'    \code{""}, which will list all resources within the director root.
#' @param by_mtime logical. Whether or not to sort results by modification time
#'    in descending order. The default is \code{TRUE}, so that the first result
#'    is the most recently modified resource.
#' @return a character vector of matched resources.
#' @examples
#' \dontrun{
#'   # Imagine we have a file structure:
#'   #   - foo
#'   #     - one
#'   #       - one.R
#'   #       - helper.R
#'   #     - two.R
#'   #
#'   # Then the bellow will return \code{"foo/one"}, \code{"two"}, and \code{""},
#'   # respectively. Note that the \code{"helper.R"} file is not considered a
#'   # resource by the director as \code{"one.R"} shares its name with its
#'   # parent directory and is considered the accessible resource.
#'
#'   d <- director('foo')
#'   d$find('fone', method = 'wildcard') # "foo/one"
#'   # Under the hood, this looks for the regex .*f.*o.*n.*e.*
#'   d$find('wo',   method = 'partial')  # "two"
#'   d$find('none', method = 'exact')    # ""
#' }
director_find <- function(search = "", method = "wildcard", base = "", by_mtime = TRUE) {
  # Definition: idempotent resources are those that share their filename
  # with the directory they reside in.
  "Look for resources by wildcard, partial, or exact matches."

  stopifnot(isTRUE(by_mtime) || identical(by_mtime, FALSE))
  if (!is.character(base)) {
    stop("In director$find, the base parameter must be a character; ",
         "instead I got a ", sQuote(class(base)[1]))
  }

  if (length(base) > 0) {
    all <- vector('list', length(base))
    for (i in seq_along(all)) {
      all[[i]] <- Recall(search = search, method = method,
                         base = base[i], by_mtime = FALSE)
      # TODO: (RK) Re-sort by modification time.
    }
    Reduce(union, all)
  } else {
    all_files <- list.files(file.path(root(), base),
                            pattern = "\\.[rR]$", recursive = TRUE)
    .find(director = .self, search = search, method = method,
          base = base, by_mtime = by_mtime)
  }
}


.find <- function(director, search, method, base, by_mtime) {
  abs_dirname <- function(x) if ((tmp <- dirname(x)) == ".") base else tmp
  all_files <- list.files(file.path(director$root(), base),
                          pattern = "\\.[rR]$", recursive = TRUE)

  # Idempotent objects are those whose filename is the same as the
  # name of the directory they reside in. This is helpful for, e.g.,
  # helper functions.
  idempotent_objects <- grep("(^|\\/)([^/]+)/\\2\\.[rR]$", all_files, value = TRUE)
  base_files <- all_files[!grepl("/", all_files, fixed = TRUE)] # TODO: (RK) Make OS-agnostic
  base_idempotent_objects <- 
    Filter(function(filename) strip_r_extension(filename) == basename(base), base_files)
  idempotent_objects <- c(idempotent_objects, base_idempotent_objects)
  idempotent_objects <- vapply(idempotent_objects, abs_dirname, character(1))

  # Find the files that belong in directories of idempotent objects --
  # that is, helper files, and exclude those from being processable
  # by this function completely.
  helper_functions <- vapply(all_files,
    function(file) is.element(abs_dirname(file), idempotent_objects), logical(1))
  all_files <- all_files[!helper_functions]
  all_files <- vapply(all_files, resource_name, character(1))

  # We now apply the filter to all files and the idempotent objects --
  # this separation is necessary to prevent things like looking for "2.1.2"
  # catching "model/2.1.1/2.1.1", which would be wrong.
  if (identical(method, "exact")) {
    return(file.path(base, Find(function(x) x == search, all_files) %||% character(0)))
  } else if (!identical(search, "")) {
    pattern <- strip_r_extension(search) # Strip file extension
    if (identical(method, "wildcard")) {
      pattern <- gsub("([]./\\*+()])", "\\\\\\1", pattern)
      pattern <- gsub("([^\\$^])", ".*\\1", pattern) # turn this into ctrl+p
      pattern <- gsub("^.*", "^", pattern, fixed = TRUE)
    }
    fixed <- identical(method, "partial")
    suppressWarnings({ # ignore.case = T with fixed = T gives harmless warning 
      all_files <- grep(pattern, all_files, fixed = fixed,
                        value = TRUE, ignore.case = TRUE)
      idempotent_objects <- idempotent_objects[
        grep(pattern, idempotent_objects,
             fixed = fixed, FALSE, ignore.case = TRUE)]
    })
  }

  # Finally, put the results together: we were looking for either
  # non-idempotent or idempotent objects passing the filter, being careful
  # to not use the whole path of the latter.
  all_files <- unname(c(all_files, idempotent_objects))

  if (identical(by_mtime, TRUE) && length(all_files) > 0) {
    descending_by_modification_time <- -vapply(all_files,
              function(f) file.info(director$.filename(file.path(base, f)))$mtime, numeric(1))
    all_files <- all_files[order(descending_by_modification_time)]
  }

  gsub("//", "/", fixed = TRUE, file.path(base, all_files)) # TODO: (RK) Handle base better here
}

