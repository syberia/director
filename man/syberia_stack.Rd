\name{syberia_stack}
\alias{syberia_stack}
\title{A helper interface for a stack of syberia resources.}
\usage{
syberia_stack(value, all = FALSE)
}
\arguments{
  \item{value}{ANY. Push a value onto the stac.}

  \item{all}{logical. Whether or not to pop and return all
  values from the stack. The default is \code{FALSE}.}
}
\value{
the popped value(s)
}
\description{
This function is meant to be used by syberia for pushing
and popping resource reference onto a stack, e.g., to keep
track of dependencies for a given syberia model.
}
\details{
This function is meant to be used as follows. Calling
\code{syberia_stack()} pops the latest value off the stack.
If no value exists, the stack gets created in the
syberiaStructure cache with key 'resource_stack' if it is
not present, or otherwise returns NULL.

If a single unnamed value is passed, like
\code{syberia_stack("foo")}, it is pushed onto the stack
(if the stack does not exist in the cache, it gets created
extemporaneously).
}
\examples{
\dontrun{
  syberia_stack(all = TRUE) # Create a new stack
  lapply(1:5, syberia_stack) # Push 1 through 5 onto the stack
  syberia_stack(all = TRUE) # Clear the stack and return as.list(1:5)
}
}

