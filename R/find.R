#' Find resources within a director project.
#'
#' @note
#' The available search methods are:
#'
#' \describe{
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
#' @param pattern character. The resources to search for. The default is
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
director_find <- function(pattern = "", method = "wildcard", base = "", by_mtime = TRUE) {
  # Definition: idempotent resources are those that share their filename
  # with the directory they reside in.
  "Look for resources by wildcard, partial, or exact matches."

  stopifnot(isTRUE(by_mtime) || identical(by_mtime, FALSE))
  if (!is.character(base)) {
    stop("In director$find, the base parameter must be a character; ",
         "instead I got a ", sQuote(class(base)[1]))
  }

  if (length(base) > 1) {
    all <- vector('list', length(base))
    for (i in seq_along(all)) {
      all[[i]] <- Recall(search = search, method = method,
                         base = base[i], by_mtime = FALSE)
      # TODO: (RK) Re-sort by modification time.
    }
    Reduce(union, all)
  } else {
    find_(.self, pattern, method, base, by_mtime)
  }
}

find_ <- function(director, pattern, method, base, by_mtime) {
  all_files <- list.files(file.path(director$root(), base),
                          pattern = "\\.[rR]$", recursive = TRUE)
  all_files <- tools::file_path_sans_ext(all_files)
  if (!is.search_pattern(pattern)) {
    pattern <- search_pattern(pattern, method)
  }
  all_files <- apply_pattern(search_pattern("", "idempotence"), all_files)
  resources <- apply_pattern(pattern, all_files)
  if (nzchar(base)) { resources <- file.path(base, resources) }
  sort_by_mtime(resources, by_mtime, director)
}

sort_by_mtime <- function(files, by_mtime, director) {
  if (isTRUE(by_mtime)) {
    descending_by_modification_time <- -vapply(files,
      function(f) file.info(director$.filename(f, absolute = TRUE))$mtime, numeric(1))
    files[order(descending_by_modification_time)]
  } else {
    files
  }
}

