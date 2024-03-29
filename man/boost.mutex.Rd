% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutexes.R
\name{boost.mutex}
\alias{boost.mutex}
\title{Create a boost.mutex object}
\usage{
boost.mutex(sharedName = NULL, timeout = NULL, create = TRUE)
}
\arguments{
\item{sharedName}{The name of the shared resource corresponding to the
mutex.  By default a universal unique identifier is supplied.}

\item{timeout}{The amount of time (in seconds) that the mutex should try
to attempt to get a lock.  By default no timeout is supplied and the
mutex will attempt to acquire the lock indefinitely.}

\item{create}{Should the mutex be created or are we attaching to an 
existing on. Default is \code{TRUE}.}
}
\description{
This function creates a \code{boost.mutex} object.
}
\examples{
# Create a boost.mutex object with default resource name and no timeout.
x = boost.mutex()
rm(x)
gc()
}
