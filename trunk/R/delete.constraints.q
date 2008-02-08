delete.constraints <- function(lprec, constraints)
{
  invisible(.Call("RlpSolve_del_constraints", lprec, as.integer(constraints)))
}

