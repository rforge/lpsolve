\name{lp.control}
\alias{lp.control}

\title{lpSolve Control Parameters}

\description{
Set control parameters in an lpSolve linear programming model.
}

\usage{
lp.control(lprec, ..., reset = FALSE)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{\dots}{control arguments to bet set in \code{lprec}.}
  \item{reset}{a logical value. If \code{TRUE} all control parameters are reset to their default values.}
}

\value{
a list containing all of the control parameters as set internally in \code{lprec}.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\keyword{programming}

