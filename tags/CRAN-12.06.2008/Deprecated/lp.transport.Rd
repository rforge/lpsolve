\name{lp.transport}
\alias{lp.transport}

\title{The Transportation Problem}

\description{An integer program solving the transportation problem. Additionally, this function provides an example of building and solving a sparse integer/linear program using the lpSolve API.}

\usage{
lp.transport(cost.mat, direction = c("min", "max"), row.signs, row.rhs,
             col.signs, col.rhs, presolve = 0, compute.sens = 0,
             integers = "all")
}

\arguments{
  \item{cost.mat}{a numeric matrix containing the costs. The ij-th element is the cost of transporting one item from source i to destination j.}
  \item{direction}{a character string specifying the direction of optimization: "min" (default) or "max."}
  \item{row.signs}{a character vector specifying the types of the row constraints: each element must be one of "<=", "=", or ">=".}
  \item{row.rhs}{a numeric vector containing the right-hand side of the row constraints.}
\item{col.signs}{a character vector specifying the types of the column constraints: each element must be one of "<=", "=", or ">=".}
  \item{col.rhs}{a numeric vector containing the right-hand side of the column constraints.}
  \item{presolve}{presolve? Default 0 (no); any non-zero value means yes.}
  \item{compute.sens}{compute sensitivity? Default 0 (no); any non-zero value means yes.}
  \item{integers}{a numeric vector of positive integers containing the indices of the decision variables that are restricted to integer values. Additionally, \code{"all"} (the default) restricts all decision variables to integer values and \code{NULL} sets all the decision variables to type real.}
}

\details{
This is a particular integer programming problem. All the decision variables are assumed to be integers, and there is one constraint per row and one per column (and no others). This is assumed to be a minimization problem.
}

\value{
An \code{\link{lp}} object. Constraints are implicit and not returned. See documentation for details.
}

\references{Example problem from Bronson (1981), \emph{Operations Research},
Scahum's Outline Series, McGraw-Hill.}

\author{Sam Buttrey, \email{sebuttre@nps.edu}}

\examples{
#
# Transportation problem, Bronson, problem 9.1, p. 86
#
# Set up cost matrix
#
costs <- matrix (10000, 8, 5); costs[4,1] <- costs[-4,5] <- 0
costs[1,2] <- costs[2,3] <- costs[3,4] <- 7; costs[1,3] <- costs[2,4] <- 7.7
costs[5,1] <- costs[7,3] <- 8; costs[1,4] <- 8.4; costs[6,2] <- 9
costs[8,4] <- 10; costs[4,2:4] <- c(.7, 1.4, 2.1)
#
# Set up constraint signs and right-hand sides.
#
row.signs <- rep ("<=", 8)
row.rhs <- c(200, 300, 350, 200, 100, 50, 100, 150)
col.signs <- rep (">=", 5)
col.rhs <- c(250, 100, 400, 500, 200)
#
# Run
#
lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)
\dontrun{Success: the objective function is 7790}
lp.transport (costs, "min", row.signs, row.rhs, col.signs, col.rhs)$solution
\dontrun{
     [,1] [,2] [,3] [,4] [,5]
[1,]    0  100    0  100    0
[2,]    0    0  300    0    0
[3,]    0    0    0  350    0
[4,]  200    0    0    0    0
[5,]   50    0    0    0   50
[6,]    0    0    0    0   50
[7,]    0    0  100    0    0
[8,]    0    0    0   50  100
}
}

\keyword{optimize}


