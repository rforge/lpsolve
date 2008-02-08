get.rhs <- function(lprec, rows = NULL)
{
  .Call("RlpSolve_get_rh", lprec, as.integer(row))
}

