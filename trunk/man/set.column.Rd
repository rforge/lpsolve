\name{set.column}
\alias{set.column}

\title{Set Column}

\description{
Set a column in an lpSolve linear programming model.
}

\usage{
set.column(lprec, column, x, indices = NULL)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{column}{a single numeric value from the set \code{\{1, \dots, n\}} specifying which column to set.}
  \item{x}{a numeric vector containing the elements (only the nonzero elements if \code{indices} is also given) to be used in the added column. The length of \code{x} must be equal to the number of constraints in \code{lprec} unless \code{indices} is provided.}
  \item{indices}{a numeric vector the same length as \code{x} of unique values from the set \{1, \dots, m\} where m is the number of constraints in \code{lprec}; \code{x[i]} is entered into constraint \code{indices[i]} in the column. The coefficients for the constraints not in \code{indices} are set to zero.}
}

\value{
a logical value indicating whether the column was successfully set is invisibly returned.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(4, 2)

x <- c(6,2,4,9)
set.column(lps.model, 2, x)

y <- c(3,1,5)
ind <- c(1,2,4)
set.column(lps.model, 1, y, ind)
}

\keyword{programming}

