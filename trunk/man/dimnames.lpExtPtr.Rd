\name{dimnames.lpExtPtr}
\alias{dimnames.lpExtPtr}
\alias{dimnames<-.lpExtPtr}

\title{Dimnames of an Object}

\description{
Retrieve or set the dimnames in an lpSolve linear programming model.
}

\usage{
\method{dimnames}{lpExtPtr}(x)
\method{dimnames}{lpExtPtr}(x) <- value
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{value}{a list containing two character vectors of lengths \code{m} and \code{n} specifying the row names and the column names for the lpSolve linear programming model.  The number of constraints in \code{lprec} is denoted by \code{m} and the number of decision variables by \code{n}.}
}

\value{
a list of two character vectors containing the row names and the column names in the lpSolve linear programming model.
}

\references{LPSOLVE_CAPI_REFERENCE}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lp <- make.lp(4, 0)

x <- c(6,2,4,9)
add.column(lp, x)

y <- c(3,1,5)
ind <- c(1,2,4)
add.column(lp, y, ind)

dimnames(lp) <- list(c("alpha", "bravo", "charlie", "delta"),
                     c("whiskey", "tango"))
}

\keyword{programming}

