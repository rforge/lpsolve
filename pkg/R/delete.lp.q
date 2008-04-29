delete.lp <- function(lprec)
{
  .Call("RlpSolve_delete_lp", lprec, PACKAGE = "lpSolve")
  invisible(lprec)
}


