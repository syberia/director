\name{syberia_root}
\alias{syberia_root}
\title{Determine the root path of the current Syberia project.}
\usage{
syberia_root(filename = NULL, error = FALSE)
}
\arguments{
  \item{filename}{character. If specified, it will attempt
  to find the Syberia root relative to the file name by
  traversing up its parent directories.  If not given,
  Syberia will try to intelligently discern the current
  Syberia project by first looking at the cache for the
  previously used Syberia project and then by looking at
  the current directory. If no project is found, the last
  resort is to look for a \code{syberia.root} option. If
  not found, this will return \code{NULL}.}

  \item{error}{logical. If \code{TRUE}, it will return an
  error if the path is not found.}
}
\value{
see the \code{filename} parameter
}
\description{
The root of the Syberia project is considered to be the
directory containing the \code{syberia.config} file.
}

