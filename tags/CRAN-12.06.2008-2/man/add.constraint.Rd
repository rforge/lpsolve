\name{add.constraint}
\alias{add.constraint}

\title{Add Constraint}

\description{
Add a constraint to an lpSolve linear program model object.
}

\usage{
add.constraint(lprec, xt, type = c("<=", "=", ">="), rhs, indices = 1:n, lhs)
}

\arguments{
  \item{lprec}{an lpSolve linear program model object.}
  \item{xt}{a numeric vector containing the constraint coefficients (only the nonzero coefficients if \code{indices} is also given). The length of \code{xt} must be equal to the number of decision variables in \code{lprec} unless \code{indices} is provided.}
  \item{type}{a numeric or character value from the set \code{\{1 = "<=", 2 = ">=", 3 = "="\}} specifying the type of the constraint.}
  \item{rhs}{a single numeric value specifying the right-hand-side of the constraint.}
  \item{indices}{a numeric vector the same length as \code{xt} of unique values from the set \{1, \dots, n\} where n is the number of decision variables in \code{lprec}; \code{xt[j]} becomes the constraint coefficient for variable \code{indices[j]}. The constraint coefficients for the decision variables not in \code{indices} are set to zero.}
  \item{lhs}{optional. A single numeric value specifying the left-hand-side of the constraint.}
}

\details{
Specifying the objective function before adding constraints will improve the performance of this function.

The use of this function should be avoided when possible. Building a model column-by-column rather than row-by-row will be on the order of 50 times faster (building the model - not solving the model).
}

\value{
a logical value is invisibly returned: \code{TRUE} indicates that the operation was successful and \code{FALSE} indicates that an error occurred.
}

\references{\url{http://lpsolve.sourceforge.net/5.5/index.htm}}

\author{Kjell Konis \email{kjell.konis@epfl.ch}}

\examples{
lps.model <- make.lp(0, 4)
set.objfn(lps.model, rep(1, 4))

xt <- c(6,2,4,9)
add.constraint(lps.model, xt, "<=", 50)

yt <- c(3,1,5)
ind <- c(1,2,4)
add.constraint(lps.model, yt, 2, 75, ind)
}

\keyword{programming}


