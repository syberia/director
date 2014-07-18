\name{syberia_data_sources}
\alias{syberia_data_sources}
\title{Find all the data sources in a Syberia project.}
\usage{
syberia_data_sources(pattern = "", type = "sources",
  root = syberia_root(), by_mtime = TRUE, fixed = FALSE)
}
\arguments{
  \item{pattern}{character. A set of characters by which to
  filter.  This uses the same format as the popular ctrl+p
  plugin for vim.  Namely, it will look for adjacent
  instances of such characters regardless of any
  interpolating characters. For example, 'ace' will match
  'abcde' but also 'abcdfghe' but not 'aebcd'.}

  \item{type}{character. This can be either
  \code{"sources"} or \code{"test"}.  The default is the
  former, whereas the latter will fetch data source tests.}

  \item{root}{character. The root of the syberia project.
  The default is \code{syberia_root()}.}

  \item{by_mtime}{logical. Whether or not to sort the data
  sources in descending order by last modified time. The
  default is \code{TRUE}.}

  \item{fixed}{logical. Whether or not to use smart
  interpolation, like in the description for the
  \code{pattern} argument. If \code{TRUE}, only substring
  matching is used.}
}
\value{
a list of filenames containing syberia data sources
}
\description{
The convention is that data source files have the same name
as the directory they are contained in (this allows other
files in the same directory to be used as helper files). If
no such file exists in a directory, all files are assumed
to be data source files. For example, if we have
}
\details{
some_data_type/data_source1.r
some_data_type/data_source2/data_source2.r
some_data_type/data_source2/helpers.r

only the first two will be considered to be data sources.
If there was a some_data_type/some_data_type.r, then the
first would no longer be considered a model object (it
would be considered a file with helper functions).
}
\seealso{
\code{\link{syberia_root}}
}

