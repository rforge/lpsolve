\name{lp}
\alias{lp}

\title{Linear and Integer Programming}

\description{Interface to the lp\_solve Linear/Integer Program Solver. This function is provided in this version of the lpSolve package mainly for backward compatibility but also as an example of programming with the lpSolve API. For dense linear programs, the function \code{\link{lpSolve}} provides better performance and several additional features. For sparse linear programs, far better performance can be achieved by building and solving the linear program using the functions provided in the lpSolve API.}

\usage{
lp(direction = c("min", "max"), objective.in, const.mat, const.dir, const.rhs,
	transpose.constraints = TRUE, int.vec, presolve = 0, compute.sens = 0)
}

\arguments{
  \item{direction}{a character string specifying the direction of optimization: "min" (default) or "max."}
  \item{objective.in}{a numeric vector containing the coefficients of the objective function.}
  \item{const.mat}{a numeric matrix containing the constraint coefficients, one row 
per constraint, one column per decision variable.}
  \item{const.dir}{a character vector specifying the types of constraints: each element must be one of "<=", "=", or ">=".}
  \item{const.rhs}{a numeric vector containing the right-hand sides of the constraints.}
  \item{transpose.constraints}{this argument is present only for backwards compatibility.}
  \item{int.vec}{a numeric vector of positive integer values specifying the indices of the decision variables that are required to be integer. The length of this vector will therefore be the number of integer decision variables.}
  \item{presolve}{currently ignored.}
  \item{compute.sens}{compute sensitivity? Default 0 (no); any non-zero value means yes.}
}

\value{
nn lp object. See \code{\link{lp.object}} for details.
}

\author{Sam Buttrey, \email{sebuttre@nps.edu}}

\seealso{\code{\link{lp.assign}}, \code{\link{lp.transport}}}

\examples{
#
# Set up problem: maximize
#   x1 + 9 x2 +   x3 subject to
#   x1 + 2 x2 + 3 x3 <=  9
# 3 x1 + 2 x2 + 2 x3 <= 15
#
f.obj <- c(1, 9, 3)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)
\dontrun{Success: the objective function is 40.5}
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
\dontrun{[1] 0.0 4.5 0.0}
#
# Get sensitivities
#
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
\dontrun{[1] -1e+30  2e+00 -1e+30}
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to  
\dontrun{[1] 4.50e+00 1.00e+30 1.35e+01}
#
# Right now the dual values for the constraints and the variables are
# combined, constraints coming first. So in this example...
#
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals     
\dontrun{[1]   4.5   0.0  -3.5   0.0 -10.5}
#
# ...the duals of the constraints are 4.5 and 0, and of the variables,
# -3.5, 0.0, -10.5. Here are the lower and upper limits on these:
#
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
\dontrun{[1]  0e+00 -1e+30 -1e+30 -1e+30 -6e+00}
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to  
\dontrun{[1] 1.5e+01 1.0e+30 3.0e+00 1.0e+30 3.0e+00}
#
# Run again, this time requiring that all three variables be integer
#
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3)
\dontrun{Success: the objective function is 37}
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3)$solution
\dontrun{[1] 1 4 0}
#
# You can get sensitivities in the integer case, but they're harder to
# interpret.
#
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3, compute.sens=TRUE)$duals
\dontrun{[1] 1 0 0 7 0}
}

\keyword{optimize}
