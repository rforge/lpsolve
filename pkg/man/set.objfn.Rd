\name{set.objfn}
\alias{set.objfn}

\title{Set Objective Function}

\description{
Set the objective function in an lpSolve linear program model object.
}

\usage{
set.objfn(lprec, obj, indices = 1:n)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{obj}{a numeric vector of length \code{n} (where n is the number of decision variables in \code{lprec}) containing the coefficients of the objective function. Alternatively, if \code{indices} is also provided, a numeric vector of the same length as \code{indices} containing only the nonzero coefficients.}
  \item{indices}{a numeric vector containing unique elements from the set \{1, \dots, n\} (where n is the number of decision variables in \code{lprec}) identifying the nonzero coefficients in the objective function.  Coefficients not specified in \code{indices} are set to zero.}
}

\value{
a logical value indicating whether setting the objective function was successful is invisibly returned.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\seealso{\code{\link{resize.lp}}}

\examples{
lps.model <- make.lp(4, 2)
x <- c(6,2,4,9)
set.column(lps.model, 2, x)
y <- c(3,1,5)
ind <- c(1,2,4)
set.column(lps.model, 1, y, ind)

set.objfn(lps.model, c(-3, -4))
}

\keyword{programming}


