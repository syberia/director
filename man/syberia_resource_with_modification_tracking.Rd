\name{syberia_resource_with_modification_tracking}
\alias{syberia_resource_with_modification_tracking}
\title{Fetch a syberia resource with modification tracking.}
\usage{
syberia_resource_with_modification_tracking(filename, root,
  check_helpers = TRUE, ...)
}
\arguments{
  \item{filename}{character. See the \code{filename}
  parameter of \code{syberia_resource}.}

  \item{root}{character. See the \code{root} parameter of
  \code{syberia_resource}.}

  \item{check_helpers}{logical. If the resource is in a
  directory with the same name as the resource, check all
  of its helpers for modifications as well.}

  \item{...}{same as arguments to \code{syberia_resource}}
}
\description{
Modification tracking refers to determining whether the
resource has changed since its last execution by syberia.
This is accomplished using the output of
\code{syberia_resource}, which will return a list that
includes modification time of the last time syberia
encountered the resource. If a modification is spotted, the
Syberia cache entry `runtime/any_modified` will be set to
\code{TRUE}.
}
\seealso{
\code{\link{syberia_resource}}
}

