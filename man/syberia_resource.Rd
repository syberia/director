\name{syberia_resource}
\alias{syberia_resource}
\title{Fetch info about a file relative to a syberia directory.}
\usage{
syberia_resource(filename, root = syberia_root(), provides = list(),
  body = TRUE, soft = FALSE, ...)
}
\arguments{
  \item{filename}{character. The filename of the R script
  to execute relative to a syberia project.}

  \item{root}{character. The root of the syberia project in
  which to look for \code{filename}. The default is
  \code{syberia_root()}.}

  \item{provides}{list or environment. A list or
  environment of values to provide to the file.}

  \item{body}{logical. Whether or not the fetch the body of
  the file in the `current` and `cached` output lists.}

  \item{soft}{logical. Whether or not to modify the cache
  to reflect the file modification time and other details.}

  \item{...}{additional arguments to pass to the
  \code{base::source} function that gets executed when the
  `value` is accessed.}
}
\value{
a four-element list with names `current`, `cached`,
`value`, and `modified`. The former two will both be
two-element lists containing keys `info` and `body` (unless
Syberia has never executed the file before in which case
the latter will be \code{NULL}). The `info` key holds the
result of calling R's built-in \code{file.info} on the file
(and includes information like created at and modified at
time), whereas the `body` key holds a character
representation of the body of the file (useful for
comparing if any actual modifications have been made).

The `value` key in the returned list holds a function which
will execute the given file in the \code{provides}
environment. A function is returned instead of the actual
value to let the caller control whether or not the file is
executed (e.g., based on its properties in the `current`
list). Most frequently, this means consulting the
`modified` key of the returned list, which will hold a
logical indicating whether or not the file has been
modified since last executed by Syberia (if this was never
the case, `modified` will be \code{FALSE}).
}
\description{
Some additional information will be returned. Namely, a
list with four keys, `current`, `cached`, `value`, and
`modified`. The former two will contain information
relating to the current and previous execution of this
file, while `value` contains a function that will execute
the file and return its output, and `modified` is a logical
flag indicating whether or not the file has been modified
since it was last executed by Syberia.
}
\details{
Internally, the cached information about the file is stored
in the syberia project's registry under the key
\code{resources/resource_cache}.
}

