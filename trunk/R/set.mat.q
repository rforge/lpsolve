set.mat <- function(lprec, row, column, value)
  invisible(.Call("RlpSolve_set_mat", lprec, as.integer(row),
                   as.integer(column), as.double(value)))

