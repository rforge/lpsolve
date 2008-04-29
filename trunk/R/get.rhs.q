get.rhs <- function(lprec, constraints = 1:m)
{
  m <- dim(lprec)[1]
  .Call("RlpSolve_get_rh", lprec, as.integer(constraints), PACKAGE = "lpSolve")
}


