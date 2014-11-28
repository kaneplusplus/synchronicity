\name{lock, lock.shared, unlock}
\Rdversion{1.1}
\alias{lock}
\alias{lock.shared}
\alias{unlock}
\title{Lock and unlock a mutex}
\description{The \code{lock} and \code{unlock} functions allow a user to
specify exclusive or shared access to a resource.}
\usage{lock(m, \dots)
lock.shared(m, \dots)
unlock(m, \dots)}
\arguments{
  \item{m}{an object derived from class mutex.}
  \item{\dots}{options associated with the mutex being used including 
\code{block} which forces the mutex to return immediately after trying
to acquire a lock}
}
\details{
A call to \code{lock} gives exclusive access to a resource; no other
mutex may acquire a lock.  A call to to \code{lock.shared} allows other
mutexes to acquire a shared lock on the resource.  When shared lock is
called while a exclusive lock has been acquired, the shared lock will 
block until the exclusive lock is release.  Likewise, if an exclusive lock
is called while a shared lock has been acquired, the exclusive lock will
block until the shared lock is released.
}
\value{
The function returns \code{TRUE} if the lock is successfully called and
\code{FALSE} otherwise
}
\author{Michael J. Kane <bigmemoryauthors@gmail.com>}
\examples{
  m = boost.mutex()
  lock(m)
  # Some code that needs to be synchronized...
  unlock(m)
}
\keyword{programming}
\keyword{misc}
