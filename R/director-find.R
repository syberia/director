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
  ## [A reference class docstring](http://stackoverflow.com/a/5931576/2540303)
  "Look for resources by wildcard, partial, or exact matches."

  enforce_type(base, "character", "director$find")

  ## If multiple base paths are provided, union together the results of 
  ## calling find on each base individually.
  if (length(base) > 1) {
    ## [The R Inferno](http://www.burns-stat.com/pages/Tutor/R_inferno.pdf)
    ## recommends pre-allocating vectors for performance benefits.
    all <- vector('list', length(base))
    for (i in seq_along(all)) {
      ## Although we need to do this instead of using `lapply`
      ## so we can take advantage of [`Recall`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Recall.html).
      all[[i]] <- Recall(pattern = pattern, method = method,
                         base = base[i], by_mtime = FALSE)
      # TODO: (RK) Re-sort by modification time: https://github.com/robertzk/director/issues/19
    }
    ## Using `c` with `recursive = TRUE` will collapse the list of character 
    ## vectors into a single character vector.
    c(all, recursive = TRUE)
  } else {
    ## This function is already getting too complex, so delegate the
    ## job of actually finding resources that match this `pattern`
    ## to a helper function.
    ## 
    ## The keyword `self` is an [R6 class keyword](https://github.com/wch/R6)  
    ## that represents the environment containing this R6 object. It is
    ## similar to other languages' `this` keyword.
    find_(self, pattern, method, base, by_mtime)
  }
}

find_ <- function(director, pattern, method, base, by_mtime) {
  ## Listing all the files below will be slow if we need to look
  ## for an exact match, so we implement a separate helper for
  ## finding exact matches.
  if (identical(tolower(method), "exact")) {
    return(exact_find(director, pattern, base))
  }

  all_files <- list.files(file.path(director$root(), base),
                          pattern = "\\.[rR]$", recursive = TRUE)
  all_files <- strip_r_extension(all_files)

  ## We will use the `apply_pattern` helper below, so all patterns should
  ## be `search_pattern` S3 objects.
  if (!is.search_pattern(pattern)) {
    pattern <- search_pattern(pattern, method)
  }

  ## First, remove helper files from consideration.
  all_files <- apply_pattern(search_pattern("", "idempotence"), all_files)
  
  ## Now filter to those files that satisfy the pattern.
  ## For example, if we used `pattern = search_pattern("foo", "partial")`,
  ## we would find files that contain "foo" as a substring.
  resources <- apply_pattern(pattern, all_files)

  if (nzchar(base)) resources <- file.path(base, resources)
  sort_by_mtime(resources, by_mtime, director)
}

sort_by_mtime <- function(files, by_mtime, director) {
  if (isTRUE(by_mtime)) {
    modified_time <- function(file) {
      ## `director$filename(..., absolute = TRUE)` will convert 
      ## the resource name into a full file path.
      file.info(director$filename(file, absolute = TRUE,
                                  check.exists = FALSE))$mtime
    }

    by_modification_time <- vapply(files, modified_time, numeric(1))
    files[order(by_modification_time, decreasing = TRUE)]
  } else {
    files
  }
}

exact_find <- function(director, pattern, base) {
  if (nzchar(base)) pattern <- file.path(base, pattern)

  ## A resource "foo" can correspond to either "foo.r", "foo.R",
  ## "foo/foo.r", or "foo/foo.R".
  candidates <- as.character(t(outer(
    c(pattern, file.path(pattern, basename(pattern))),
    c(".r", ".R"), paste0
  )))

  ## However, if it is "foo.r" or "foo.R", we must ensure it is not
  ## the helper of an idempotent resource. If the resource is 
  ## prefixed by, say, "bar", we check "bar/bar.r" and "bar/bar.R"
  ## in addition to "bar/foo.r" and "bar/foo.R".
  to_idempotent <- function(f) {
    file.path(dirname(f), paste0(basename(dirname(f)), c(".r", ".R")))
  }
  absolute_candidates <- file.path(director$root(), candidates)
  absolute_candidates <- c(absolute_candidates,
    sapply(absolute_candidates[1], to_idempotent))

  ## Batching the exists check is faster because there is only one system call.
  exists <- file.exists(absolute_candidates)

  ## If "foo.r" or "foo.R" exist but they are helpers, do not treat them
  ## as resources.
  if (any(exists[1:2]) && any(exists[5:6])) character(0) 
  ## Otherwise, if it is a proper resource, select the first match.
  else if (any(exists[1:4])) pattern
  else character(0)
}

