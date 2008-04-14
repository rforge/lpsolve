\name{lp.object}
\alias{lp.object}

\title{LP (linear programming) object}

\description{Structure of lp object}

\value{
An lp.object is a list containing the following elements:
  \item{direction}{the direction of optimization: \code{0} = minimization, \code{1} = maximization.}
  \item{x.count}{an integer: the number of variables in objective function.}
  \item{objective}{a numeric vector containing the coefficients of the objective function.}
  \item{const.count}{an integer: the number of constraints in the model.}
  \item{int.count}{an integer: the number of integer decision variables in the model.}
  \item{int.vec}{an integer vector: the indices of the integer decision variables in the model.}
  \item{objval}{a numeric value: the optimal value of the objective function.}
  \item{solution}{a numeric vector: the values of the decision variables at the optimal solution.}
  \item{status}{an integer: indicates 0 = success, 2 = no feasible solution.}
}

\author{Sam Buttrey, \email{sebuttre@nps.edu}}

\seealso{\code{\link{lp}}, \code{\link{lp.assign}}, \code{\link{lp.transport}}}

\keyword{optimize}

