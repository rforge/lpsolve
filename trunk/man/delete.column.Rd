\name{delete.column}
\alias{delete.column}

\title{Delete Column}

\description{
Delete a column from an lpSolve linear programming model.
}

\usage{
delete.column(lprec, columns)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{columns}{a numeric vector of unique values from the set \code{\{1, \dots, n\}} (where \code{n} is the number of decision variables in \code{lprec}) specifying which columns should be deleted.}
}

\value{
a logical value is invisibly returned: \code{TRUE} indicates that the operation was successful and \code{FALSE} indicates that an error occurred.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(4, 0)

x <- c(6,2,4,9)
add.column(lps.model, x)

y <- c(3,1,5)
ind <- c(1,2,4)
add.column(lps.model, y, ind)

delete.column(lps.model, 1)
}

\keyword{programming}

