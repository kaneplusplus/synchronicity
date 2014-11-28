\name{boost.mutex}
\alias{boost.mutex}
\title{ Create a boost.mutex object }
\description{
  This function creates a \code{boost.mutex} object.
}
\usage{
boost.mutex(sharedName = NULL, timeout = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sharedName}{The name of the shared resource corresponding to the
mutex.  By default a universal unique identifier is supplied.}
  \item{timeout}{The amount of time (in seconds) that the mutex should try
to attempt to get a lock.  By default no timeout is supplied and the
mutex will attempt to acquire the lock indefinitely.}
}
\value{
  This function returns a \code{boost.mutex} object.
}
\author{Michael J. Kane <bigmemoryauthors@gmail.com>}

\seealso{\code{\link{synchronicity}}}
\examples{
# Create a boost.mutex object with default resource name and no timeout.
x = boost.mutex()
}
\keyword{programming}
\keyword{misc}
