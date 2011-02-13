set.mat <- function(lprec, i, j, value)
{
  .Call("RlpSolve_set_mat", lprec, as.integer(i), as.integer(j),
         as.double(value), PACKAGE = "lpSolveAPI")

  invisible()
}


