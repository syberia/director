% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/director-find.R
\name{director_find}
\alias{director_find}
\title{Find resources within a director project.}
\usage{
director_find(pattern = "", method = "wildcard", base = "",
  by_mtime = TRUE)
}
\arguments{
\item{pattern}{character. The resources to search for. The default is
\code{""}, which will list all resources within the \code{base}.}

\item{method}{character. The search method. The available options
are \code{"wildcard"}, code{"substring"}, or \code{"exact"}. See the function
description for the full explanation of these methods. The default is
\code{"wildcard"}.}

\item{base}{character. A prefix under which to look for. For example,
if \code{base = "subdir"}, then only resources under the \code{"subdir"}
directory (relative to the director root) will be returned. The default is
\code{""}, which will list all resources within the director root.}

\item{by_mtime}{logical. Whether or not to sort results by modification time
in descending order. The default is \code{TRUE}, so that the first result
is the most recently modified resource.}
}
\value{
a character vector of matched resources.
}
\description{
Find resources within a director project.
}
\note{
The available search methods are:

\describe{
  \item{wildcard}{Similar to Sublime or vim's ctrl + P, this method
    of search will look for consecutive appearances of characters.
    For example, if we have a resource \code{"some_resource"}, then
    looking for \code{"so"}, \code{"sre"} or even \code{"smsrc"} will
    return a match, since those characters occur consecutively in the
    original resource name.}
  \item{partial}{This method will try to find a substring that
    matches the resource name. For example, if we have
    \code{"dir/some_resource"}, then looking for \code{"dir/some"} will
    return a match.}
  \item{exact}{The exact name of the resource. In this mode, either a 
    single string (the resource name itself) or \code{character(0)} will
    be returned.}
}
}
\examples{
\dontrun{
  # Imagine we have a file structure:
  #   - foo
  #     - one
  #       - one.R
  #       - helper.R
  #     - two.R
  #
  # Then the bellow will return \\code{"foo/one"}, \\code{"two"}, and \\code{""},
  # respectively. Note that the \\code{"helper.R"} file is not considered a
  # resource by the director as \\code{"one.R"} shares its name with its
  # parent directory and is considered the accessible resource.

  d <- director('foo')
  d$find('fone', method = 'wildcard') # "foo/one"
  # Under the hood, this looks for the regex .*f.*o.*n.*e.*
  d$find('wo',   method = 'partial')  # "two"
  d$find('none', method = 'exact')    # ""
}
}
