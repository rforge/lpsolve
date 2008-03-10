delete.constraint <- function(lprec, constraints)
  invisible(.Call("RlpSolve_del_constraint", lprec, as.integer(constraints)))

