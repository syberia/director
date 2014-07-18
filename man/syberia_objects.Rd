\name{syberia_objects}
\alias{syberia_objects}
\title{Find all the syberia objects of the given type and subtype in a Syberia project.}
\usage{
syberia_objects(pattern = "", base = syberia_root(), by_mtime = TRUE,
  fixed = FALSE)
}
\arguments{
  \item{pattern}{character. A set of characters by which to
  filter.  This uses the same format as the popular ctrl+p
  plugin for vim.  Namely, it will look for adjacent
  instances of such characters regardless of any
  interpolating characters. For example, 'ace' will match
  'abcde' but also 'abcdfghe' but not 'aebcd'.}

  \item{base}{character. A subdirectory to look in. For
  example, \code{type = file.path(syberia_root(),
  'models')} will look in the \code{models} subdirectory of
  the root directory of the currently active Syberia
  project, whereas the same with \code{'models/dev'} will
  look in the \code{models/dev} subdirectory. The default
  is \code{syberia_root()}.}

  \item{by_mtime}{logical. Whether or not to sort the
  models in descending order by last modified time. The
  default is \code{TRUE}.}

  \item{fixed}{logical. Whether or not to use smart
  interpolation, like in the description for the
  \code{pattern} argument. If \code{TRUE}, only substring
  matching is used.}
}
\value{
a list of filenames containing syberia objects
}
\description{
Syberia objects can refer to models, data sources, or
tests. The essence of the idea is that the \code{pattern}
parameter specifies a set of consecutive character by which
to look for, and \code{type} specifies the subdirectory (an
additional subdirectory of that directory can be set using
the \code{subtype} parameter).
}
\details{
For example, if we are looking for models in the prod
environment matching "gbm", we could try:
\code{syberia_objects('gbm', 'models', 'prod')}.

Note, however, that the first argument (\code{pattern})
does not look for a substring match, but an interpolated
match: for example, looking for 'abc' will match "a1b2c" or
"model_a/submodel_bc" but will not match "acb" or any
string where the characters 'a', 'b', and 'c' do not appear
consecutively (with arbitrary strings in between them).
}
\seealso{
\code{\link{syberia_models}}
}

