\name{get.mat}
\alias{get.mat}

\title{Get Matrix Element}

\description{
Retrieve a single element from the matrix of constraints.
}

\usage{
get.mat(lprec, row, column)
}

\arguments{
  \item{lprec}{an lpSolve linear programming model.}
  \item{row}{a single numeric value from the set \{1, \dots, m\} (where \code{m} is the number of constraints in \code{lprec}) specifying the row of the matrix element.}
  \item{column}{a single numeric value from the set \{1, \dots, n\} (where \code{n} is the number of decision variables in \code{lprec}) specifying the column of the matrix element.}
}


\value{
a single numeric value.
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

get.mat(lps.model, 2, 2)
}

\keyword{programming}

