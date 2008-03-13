\name{set.rhs}
\alias{set.rhs}

\title{Set Right-Hand-Side}

\description{
Set elements on the right-hand-side of an lpSolve linear programming model.
}

\usage{
set.rhs(lprec, b, rows = NULL)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{b}{a numeric vector of length \code{m} (where m is the number of constraints in \code{lprec}) }
  \item{rows}{a numeric vector containing unique elements from the set \{1, \dots, m\} (where m is the number of constraints in \code{lprec}) identifying the constraints for which to set the right-hand-side value. If \code{NULL} the right-hand-side is set for all constraints.}
}

\value{
a logical vector indicating whether setting each right-hand-side element was successful.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lp <- make.lp(4, 2)
x <- c(6,2,4,9)
set.column(lp, 2, x)
y <- c(3,1,5)
ind <- c(1,2,4)
set.column(lp, 1, y, ind)

set.rhs(lp, c(10, 20, 40, 80))
}

\keyword{programming}

