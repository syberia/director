\name{extensionless_exists}
\alias{extensionless_exists}
\title{Determine whether an R file exists regardless of case of extension.}
\usage{
extensionless_exists(filename)
}
\arguments{
  \item{filename}{character. The filename to test (possibly
  without extension).}
}
\value{
\code{TRUE} or \code{FALSE} if the filename exists
regardless of R extension.
}
\description{
Determine whether an R file exists regardless of case of
extension.
}
\examples{
\dontrun{
 # Assume we have a file \\code{"foo.R"}. The following all return \\code{TRUE}.
 extensionless_exists('foo.R')
 extensionless_exists('foo.r')
 extensionless_exists('foo')
}

