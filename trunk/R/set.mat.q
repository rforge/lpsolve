set.mat <- function(lprec, i, j, value)
  invisible(.Call("RlpSolve_set_mat", lprec, as.integer(i), as.integer(j),
                   as.double(value), PACKAGE = "lpSolve"))


