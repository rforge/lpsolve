delete.lp <- function(lprec)
{
  .Call("RlpSolve_delete_lp", lprec, PACKAGE = "lpSolveAPI")
  invisible(lprec)
}


