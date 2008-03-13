get.bounds <- function(lprec, columns = NULL)
{
  if(is.null(columns))
    columns <- 1:dim(lprec)[2]

  lower <- .Call("RlpSolve_get_lowbo", lprec, as.integer(columns),
                  PACKAGE = "lpSolve")
  upper <- .Call("RlpSolve_get_upbo", lprec, as.integer(columns),
                  PACKAGE = "lpSolve")

  list(lower = lower, upper = upper)
}


