\name{delete.constraint}
\alias{delete.constraint}

\title{Delete Constraint}

\description{
Delete a constraint from an lpSolve linear programming model.
}

\usage{
delete.constraint(lprec, constraints)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{constraints}{a numeric vector of unique values from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying which constraints should be deleted.}
}

\value{
a logical value is invisibly returned: \code{TRUE} indicates that the operation was successful and \code{FALSE} indicates that an error occurred.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lp <- make.lp(0, 4)
set.objfn(lp, rep(1, 4))

xt <- c(6,2,4,9)
add.constraint(lp, xt, "<=", 50)

yt <- c(3,1,5)
ind <- c(1,2,4)
add.constraint(lp, yt, 2, 75, ind)

delete.constraint(lp, 1)
}

\keyword{programming}

