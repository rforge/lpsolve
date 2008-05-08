\name{lp.assign}
\alias{lp.assign}

\title{The Assignment Problem}

\description{An integer program solving the assignment problem}

\usage{
lp.assign(cost.mat, direction = c("min", "max"), presolve = 0,
          compute.sens = 0)
}

\arguments{
  \item{cost.mat}{a numeric matrix containing the costs: the ij-th element is the cost of 
assigning source i to destination j.}
  \item{direction}{a character string specifying the direction of optimization: "min" (default) or "max."}
  \item{presolve}{currently ignored.}
  \item{compute.sens}{compute sensitivity? Default 0 (no); any non-zero value means yes.}
}

\details{
This is a particular integer programming problem. All the decision variables are assumed to be integers; each row has the constraint that its entries must add up to 1 (so that there is one 1 and the remaining entries are 0) and each column has the same constraint. This is assumed to be a
minimization problem.
}

\value{
An \code{\link{lp}} object. See documentation for details.
}

\author{Sam Buttrey, \email{sebuttre@nps.edu}}

\examples{
assign.costs <- matrix (c(2, 7, 7, 2, 7, 7, 3, 2, 7, 2, 8, 10, 1, 9, 8, 2), 4, 4)
\dontrun{
> assign.costs
     [,1] [,2] [,3] [,4]
[1,]    2    7    7    1
[2,]    7    7    2    9
[3,]    7    3    8    8
[4,]    2    2   10    2
}
lp.assign (assign.costs)
\dontrun{Success: the objective function is 8}
lp.assign (assign.costs)$solution
\dontrun{
     [,1] [,2] [,3] [,4]
[1,]    0    0    0    1
[2,]    0    0    1    0
[3,]    0    1    0    0
[4,]    1    0    0    0
}
}

\keyword{optimize}


