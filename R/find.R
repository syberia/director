#' Find resources within a director project.
#'
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
#'     single string (the resource name itself) or an empty character will
#'     be returned. Note this is functionally identical to
#'     \code{director$exists}.}
#' }
#'
#' @param search character. The resources to search for.
#' @param method character. The search method. The available options
#'    are \code{"wildcard"}, code{"substring"}, or \code{"exact"}. See the function
#'    description for the full explanation of these methods. The defailt is
#'    \code{"wildcard"}.
#' @return a character vector of matched resources.
#' \dontrun{
#'   # Imagine we have a file structure:
#'   #   - foo
#'   #     - one
#'   #       - one.R
#'   #       - helper.R
#'   #     - two.R
#'   #
#'   # Then the bellow will return \code{'foo/one'}, \code{'two'}, and \code''},
#'   # respectively. Note that the \code{"helper.R"} file is not considered a
#'   # resource by the director as \code{"one.R"} shares its name with its
#'   # parent directory and is considered the accessible resource.
#'
#'   d <- director('foo')
#'   d$find('fone', method = 'wildcard')
#'   d$find('wo',   method = 'partial')
#'   d$find('none', method = 'exact')
#'   d$exists('two')
#' }
director_find <- function(search, method = 'wildcard') {
  # Definition: idempotent resources are those that share their filename
  # with the directory they reside in.
  'Look for resources by wildcard, partial, or exact matches.'
}

