\name{describe, attach.mutex}
\Rdversion{1.1}
\alias{describe}
\alias{attach.mutex}
\title{Create descriptors to mutexes and attach}
\description{
The \code{describe} function returns information that is needed to 
``connect'' to a mutex from another process.  This connection is performed
by the \code{attach.mutex} function.}
\usage{
describe(x)
attach.mutex(obj, \dots)}
\arguments{
  \item{x}{a \code{boost.mutex} object}
  \item{obj}{a \code{boost.mutex.descriptor} object}
  \item{\dots}{other options needed to attach to a mutex.}
}
\value{
The \code{describe} function returns a \code{boost.mutex.descriptor} object.
the \code{attach.mutex} function returns a \code{boost.mutex} object
described by \code{obj}.
}
\author{Michael J. Kane <bigmemoryauthors@gmail.com>}
\examples{
  m = boost.mutex()
  mm = attach.mutex(describe(m))
  # Now, both m and mm specify the same mutex.
}
\keyword{programming}
\keyword{misc}
