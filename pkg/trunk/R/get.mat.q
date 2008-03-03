get.mat <- function(lprec, row, column)
  .Call("RlpSolve_get_mat", lprec, as.integer(row), as.integer(column))

