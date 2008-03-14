\name{set.row}
\alias{set.row}

\title{Set Row}

\description{
Set a column in an lpSolve linear programming model.
}

\usage{
set.row(lprec, row, xt, indices = NULL)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{row}{a single numeric value from the set \code{\{1, \dots, m\}} (where \code{m} is the number of constraints in \code{lprec}) specifying which column to set.}
  \item{xt}{a numeric vector containing the constraint coefficients (only the nonzero coefficients if \code{indices} is also given). The length of \code{xt} must be equal to the number of decision variables in \code{lprec} unless \code{indices} is provided.}
  \item{indices}{a numeric vector the same length as \code{xt} of unique values from the set \{1, \dots, n\} (where n is the number of decision variables in \code{lprec}); \code{xt[j]} becomes the constraint coefficient for variable \code{indices[j]}. The constraint coefficients for the decision variables not in \code{indices} are set to be zero.}
}

\value{
a logical value indicating whether the row was successfully set.
}

\references{url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(2, 4)

xt <- c(6,2,4,9)
set.row(lps.model, 2, xt)

yt <- c(3,1,5)
ind <- c(1,2,4)
set.row(lps.model, 1, yt, ind)
}

\keyword{programming}
