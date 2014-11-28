\name{boost.mutex.descriptor-class}
\Rdversion{1.1}
\docType{class}
\alias{descriptor-class}
\alias{boost.mutex.descriptor-class}
\alias{attach.mutex,boost.mutex.descriptor-method}
\alias{attach.mutex,character-method}

\title{Class ``boost.mutex.descriptor'' }
\description{	 An object of this class contains necessary and sufficient
information to ``attach'' to a \code{\link{boost.mutex}} object.}
\section{Objects from the Class}{
Objects should not be created by calls of the form
\code{new("boost.mutex.descriptor", ...)},
but should use the \code{\link{describe}} function.
}
\section{Slots}{
	 \describe{
    \item{\code{description}:}{Object of class \code{"list"}}
  }
}
\section{Extends}{
Class \code{"\linkS4class{descriptor}"}, directly.
}
\section{Methods}{
  \describe{
    \item{attach.mutex}{\code{signature(obj = "boost.mutex.descriptor")}: ... }
	 }
}
\author{Michael J. Kane <bigmemoryauthors@gmail.com>}
\seealso{\code{\link{boost.mutex}}}
\examples{
showClass("boost.mutex.descriptor")
}
\keyword{classes}
