\name{set.branch.weights}
\alias{set.branch.weights}

\title{Set Branch Weights}

\description{
Set weights on the variables in an lpSolve linear programming model.
}

\usage{
set.branch.weights(lprec, weights)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{weights}{a numeric vector with \code{n} elements (where \code{n} is the number of decision variables in \code{lprec}) containing the weights for the decision variables.}
}

\value{
a logical value indicating whether the weight were successfully set is invisibly returned.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\keyword{programming}

