\docType{package}
\name{director}
\alias{director}
\alias{director-package}
\title{Director package responsible for managing and tracking changes in a file structure.}
\description{
The director package is responsible for managing and
loading resources in some fixed directory structure. It has
support for tracking changes between consecutive loads of
resources (so that we can tell if a script was modified
since we last ran it) and defining parsers that allow us to
generalize from the pernicious simple linear execution that
is common to R.

A director is a reference class responsible for a
collection of file-traversal related responsibilities.
}
\details{
Throughout, a "resource" refers to an R script with
possible helper functions. A resource with helpers is
identified by the following heuristic: if a filename sans
extension is identical to its directory name, it is
considered the primary accessible "resource" with any
accompanying helpers (files in the same directory) natively
invisible to the directory. For example, a file called
\code{"foo.R"} residing in directory \code{"foo"} will be
accessible from the director object using
\code{director_object$resource('foo')}, but any other R
scripts in the \code{"foo"} directory will not be visible
to the director object.

With this definition of resource, a director manages all of
the following relative to a root directory.

\itemize{ \item{"Loading"}{Grabbing resources relative to
the root directory using the \code{resource} method. This
also provides information about the last time the resource
was grabbed (modification time, previous body).}
\item{"Tracking"}{Besides tracking information about the
loaded resource (and any previous versions of the loaded
resource), the director also maintains a stack of resources
that have been loaded so far. This allows one to, for
example, force clear the stack, execute some code, and have
a list of resources that were relevant to the computation.
The current stack is available with
\code{director_object$stack(all = TRUE)}, which will also
clear it.} \item{"Parsers"}{Some resources are not intended
to be merely executed, but also parsed. For example, if we
define several functions in an R script, we have access to
those functions if we had sourced it with
\code{base::source(script, local = some_environment)} as
they are now present in \code{some_environment}. A parser
is a wrapper around loading of resources that allows one to
do some further computation with this information. For
example, we may define a \code{read} and \code{write}
function, but parse this information into some IO object
when fetching the resource. Thus, a resource can function
as inputs to some parser that produces some final resource
object. Parsers can be defined for full directories or
specific files.}
}

