\name{set.constr.type}
\alias{set.constr.type}

\title{Set Constraint Type}

\description{
Set constraint types in an lpSolve linear programming model.
}

\usage{
set.constr.type(lprec, types, indices = NULL)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{type}{either a numeric vector or a character vector containing elements from the set \code{\{1 = "<=", 2 = ">=", 3 = "="\}} specifying the type of the constraint.}
  \item{indices}{a numeric vector of unique values from the set \{1, \dots, m\} (where m is the number of constraints in \code{lprec}) specifying which constraints to set.}
}

\value{
a vector of logical values indicating whether the constraint types were successfully set.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\seealso{\code{\link{resize.lp}}}

\examples{
lp <- make.lp(4, 2)

x <- c(6,2,4,9)
set.column(lp, 2, x)

y <- c(3,1,5)
ind <- c(1,2,4)
set.column(lp, 1, y, ind)

set.constr.type(lp, rep("<=", 4))
}

\keyword{programming}

