\name{add.column}
\alias{add.column}

\title{Add Column}

\description{
Add a column to an lpSolve linear programming model.
}

\usage{
add.column(lprec, x, indices = NULL)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{x}{a numeric vector containing the elements (only the nonzero elements if \code{indices} is also given) to be used in the added column. The length of \code{x} must be equal to the number of constraints in \code{lprec} unless \code{indices} is provided.}
  \item{indices}{a numeric vector the same length as \code{x} of unique values from the set \{1, \dots, m\} where m is the number of constraints in \code{lprec}; \code{x[i]} is entered into constraint \code{indices[i]} in the added column. The coefficients for the constraints not in \code{indices} are set to zero.}
}

\details{
This function adds an additional column to an lpSolve linear programming model.  If multiple columns are to be added, performance can be improved by first calling \code{\link{resize.lp}} then adding the columns.
}

\value{
a logical value is invisibly returned: \code{TRUE} indicates that the operation was successful and \code{FALSE} indicates that an error occurred.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\seealso{\code{\link{resize.lp}}}

\examples{
lps.model <- make.lp(4, 0)

x <- c(6,2,4,9)
add.column(lps.model, x)

y <- c(3,1,5)
ind <- c(1,2,4)
add.column(lps.model, y, ind)
}

\keyword{programming}

