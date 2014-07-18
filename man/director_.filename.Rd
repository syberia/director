\name{director_.filename}
\alias{director_.filename}
\title{Convert a resource to a filename.}
\usage{
director_.filename(filename, absolute = FALSE)
}
\arguments{
  \item{filename}{character. The filename to convert to a
  full path.}

  \item{absolute}{character. Whether or not to return the
  absolute path (i.e., prepended by the director root). The
  default is \code{FALSE}.}
}
\value{
the full path, relative to the director root if \code{full
= FALSE} and an absolute path if \code{FULL = TRUE}.
}
\description{
Convert a resource to a filename.
}

