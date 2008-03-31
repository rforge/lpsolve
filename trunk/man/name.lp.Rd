\name{name.lp}
\alias{name.lp}

\title{Name LP}

\description{
Set or retrieve the name of an lpSolve linear programming model.
}

\usage{
name.lp(lprec, name)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{name}{an optional character string giving a new name for \code{lprec}.}
}

\details{
If \code{name} is provided then this function sets the name of the lpSolve linear programming model. If \code{name} is missing then this function retrieves the name from \code{lprec}.
}

\value{
if \code{name} is given, a logical value indicating whether the name was successfully set. Otherwise a character string containing the name of the lpSolve linear programming model.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(0, 3)
xt <- c(6,2,4)
add.constraint(lps.model, xt, "<=", 150)
xt <- c(1,1,6)
add.constraint(lps.model, xt, ">=", 0)
xt <- c(4,5,4)
add.constraint(lps.model, xt, "=", 40)
set.objfn(lps.model, c(-3,-4,-3))

name.lp(lps.model, "Simple LP")
name.lp(lps.model)
}

\keyword{programming}

