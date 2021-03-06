% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{enforce_type}
\alias{enforce_type}
\title{Enforce parameter types (logical, character, etc.).}
\usage{
enforce_type(object, admissible_types, function_name,
  name = deparse(substitute(object)))
}
\arguments{
\item{object}{ANY. An R object to enforce types on.}

\item{admissible_types}{character. A character vector of allowed types.}

\item{function_name}{character. The function this enforcement is occurring
in, for error messages.}

\item{name}{character. The name of the parameter whose type is being
enforced. By default, the string expression passed in to the first
argument, \code{object}.}
}
\value{
Nothing, but error if the type does not match.
}
\description{
Enforce parameter types (logical, character, etc.).
}
\examples{
\dontrun{
x <- 1
enforce_type(x, "logical", "myfunction")
# Will call stop() with the following error:
# "In 'myfunction', the 'x' parameter must be a character; instead, I got
# a logical.
}
}
