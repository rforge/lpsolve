get.constr.value <- function(lprec, side = c("rhs", "lhs"), constraints = 1:m)
{
  m <- dim(lprec)[1]
  side <- match.arg(side)

  if(m < 1)
    constraints <- integer(0)

  value <- .Call("RlpSolve_get_rh", lprec, as.integer(constraints),
                  PACKAGE = "lpSolve")

  if(side == "lhs") {
    constr.types <- get.constr.type(mps, constraints = constraints,
                                    as.char = FALSE)
    range <- .Call("RlpSolve_get_rh_range", lprec, as.integer(constraints),
                    PACKAGE = "lpSolve")
    range[constr.types == 1] <- -range[constr.types == 1]
    value <- value + range
  }

  value
}


