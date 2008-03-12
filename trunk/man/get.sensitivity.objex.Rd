\name{get.sensitivity.objex}
\alias{get.sensitivity.objex}

\title{Get Sensitivity: Objective Extended}

\description{
Retrieve the sensitivity of the objective function from a successfully solved lpSolve linear programming model.
}

\usage{
get.sensitivity.objex(lprec)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
}

\value{
a list with components

  \item{objfrom}{a numeric vector of length \code{n} (where \code{n} is the number of decision variables in \code{lprec}) containing the values of the lower limits of the objective function.}

  \item{objtill}{a numeric vector of length \code{n} (where \code{n} is the number of decision variables in \code{lprec}) containing the values of the upper limits of the objective function.}

  \item{objfromvalue}{a numeric vector of length \code{n} (where \code{n} is the number of decision variables in \code{lprec}) containing the values of the variables at their lower limit. Only applicable when the value of the variable is 0 (rejected).}

  \item{objtillvalue}{not used in this version of lp_solve.}
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lp <- make.lp(0, 3)
xt <- c(6,2,4)
add.constraint(lp, xt, "<=", 150)
xt <- c(1,1,6)
add.constraint(lp, xt, ">=", 0)
xt <- c(4,5,4)
add.constraint(lp, xt, "=", 40)
set.objfn(lp, c(-3,-4,-3))

solve(lp)
get.sensitivity.objex(lp)
}

\keyword{programming}

