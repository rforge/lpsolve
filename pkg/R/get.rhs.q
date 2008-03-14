get.rhs <- function(lprec, rows = NULL)
{
  if(is.null(rows))
    rows <- 1:dim(lprec)[1]

  .Call("RlpSolve_get_rh", lprec, as.integer(rows), PACKAGE = "lpSolve")
}

