lp <- function(direction = c("min", "max"), objective.in, const.mat, const.dir,
               const.rhs, transpose.constraints = TRUE, int.vec, presolve = 0,
               compute.sens = 0)
{
	# lp: solve a general linear program
	#
	# Arguments:
	#     direction: Character: direction of optimization: "min" (default) or
  #                "max."
	#  objective.in: Numeric vector (or one-column data frame) of coefficients
  #                of objective function
	#     const.mat: Matrix of numeric constraint coefficients, one row  per
  #                constraint, one column per variable (unless
  #                transpose.constraints =  FALSE; see below).
	#     const.dir: Vector of character strings giving the direction of the
  #                constraints: each value should be one of "<=", "=" or ">=."
	#     const.rhs: Vector of numeric values for the right-hand sides of  the
  #                constraints.
	# transpose.constraints: By default each constraint occupies a row  of
  #                const.mat, and that matrix needs to be transposed before
  #                being passed  to the optimizing code.  For very large
  #                constraint matrices it may be wiser  to construct the
  #                constraints in a matrix column-by-column. In that case set
  #                transpose.constraints to FALSE.
	#       int.vec: Numeric vector giving the indices of variables that are
  #                required to be integer. The length of this vector will
  #                therefore be the  number of integer variables.
	#	 presolve: Numeric: Should presolve be done (in lp_solve)? Default: 0 (no).
	#                A non-zero value means "yes." Currently mostly ignored.
	#  compute.sens: Numeric: compute sensitivities? Default 0 (no). Any non-zero
	#                value means "yes."

  if(!transpose.constraints)
    const.mat <- t(const.mat)

  direction <- match.arg(direction)

  n <- dim(const.mat)[1]
  p <- dim(const.mat)[2]
  one2n <- as.integer(1:n)

  # Basic usage checks

  if(length(objective.in) != p)
    stop(sQuote("objective.in"), " must have the same number of elements as ",
         "there are columns in ", sQuote("const.mat"))

  if(length(const.dir) != n)
    stop(sQuote("const.dir"), " must have the same number of elements as ",
         "there are rows in ", sQuote("const.mat"))

  if(length(const.rhs) != n)
    stop(sQuote("const.rhs"), " must have the same number of elements as ",
         "there are rows in ", sQuote("const.mat"))

  # The following is for consistency with the old version of lpSolve lp objects

	const.dir.num <- rep(-1, length(const.dir))
	const.dir.num[const.dir == "<" | const.dir == "<="] <- 1
	const.dir.num[const.dir == "=" | const.dir == "=="] <- 3
	const.dir.num[const.dir == ">" | const.dir == ">="] <- 2
	if(any(const.dir.num == -1))
		stop("Unknown constraint direction found\n")

  big.const.mat <- rbind(t(const.mat), const.dir.num, const.rhs)

	if(missing(int.vec)) {
		int.count <- 0
		int.vec <- 0
	}
	else {
		int.count <- length(int.vec)
	}

  # Build the model

  lp <- make.lp(n, p)
  control <- lp.control(lp, sense = direction)
  for(j in 1:p)
    set.column(lp, j, const.mat[, j], one2n)
  set.rhs(lp, const.rhs, one2n)
  set.constr.type(lp, const.dir.num, one2n)
  set.objfn(lp, objective.in)
  if(int.vec[1] > 0)
    set.type(lp, int.vec, "integer")
  if(compute.sens > 0)
    control <- lp.control(lp, presolve = "sensduals")

  # Solve the model

  status <- solve(lp)

  lp.out <- list(direction = as.integer(ifelse(direction == "min", 0, 1)),
                 x.count = as.integer(p),
                 objective = objective.in,
                 const.count = as.integer(n),
                 constraints = big.const.mat,
                 int.count = as.integer(int.count),
                 int.vec = as.integer(int.vec),
                 objval = as.double(get.objective(lp)),
                 solution = as.double(get.variables(lp)),
                 presolve = as.integer(0),
                 compute.sens = as.integer(compute.sens),
                 sens.coef.from = double(1),
                 sens.coef.to = double(1),
                 duals = double(1),
                 duals.from = double(1),
                 duals.to = double(1),
                 status = as.integer(status))

  if(compute.sens > 0) {
    sens.obj <- get.sensitivity.obj(lp)
    sens.rhs <- get.sensitivity.rhs(lp)
    lp.out$sens.coef.from = as.double(sens.obj$objfrom)
    lp.out$sens.coef.to = as.double(sens.obj$objtill)
    lp.out$duals = as.double(sens.rhs$duals)
    lp.out$duals.from = as.double(sens.rhs$dualsfrom)
    lp.out$duals.to = as.double(sens.rhs$dualstill)
  }

  if(any(names(version) == "language"))
    class(lp.out) <- "lp"
  else
    oldClass(lp.out) <- "lp"

	lp.out
}

