get.bounds <- function(lprec, columns = 1:n)
{
  n <- dim(lprec)[2]

  if(n < 1)
    columns <- integer(0)

  lower <- .Call("RlpSolve_get_lowbo", lprec, as.integer(columns),
                  PACKAGE = "lpSolveAPI")
  upper <- .Call("RlpSolve_get_upbo", lprec, as.integer(columns),
                  PACKAGE = "lpSolveAPI")

  list(lower = lower, upper = upper)
}


