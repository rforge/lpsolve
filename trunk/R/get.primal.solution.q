get.primal.solution <- function(lprec)
  .Call("RlpSolve_get_primal_solution", lprec, PACKAGE = "lpSolve")


