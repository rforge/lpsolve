get.bounds <- function(lprec, columns = 1:n)
{
  n <- dim(lprec)[2]

  lower <- .Call("RlpSolve_get_lowbo", lprec, as.integer(columns),
                  PACKAGE = "lpSolve")
  upper <- .Call("RlpSolve_get_upbo", lprec, as.integer(columns),
                  PACKAGE = "lpSolve")

  list(lower = lower, upper = upper)
}


