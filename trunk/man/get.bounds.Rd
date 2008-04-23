\name{get.bounds}
\alias{get.bounds}

\title{Get Bounds}

\description{
Retrieve the bounds on the decision variables from an lpSolve linear program model object.
}

\usage{
get.bounds(lprec, columns = NULL)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying for which decision variables the bounds should be retrieved. If \code{NULL}, the bounds are retrieved for all the decision variables.}
}

\value{
  a list with components \code{columns}, \code{lower} and \code{upper}.  The bounds on the decision variable in \code{columns[j]} are \code{lower[j]} and \code{upper[j]}.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(3, 3)
get.bounds(lps.model)
}

\keyword{programming}

