get.constr.types <- function(lprec, constraints = NULL, char = TRUE)
{
  if(is.null(constraints))
    constraints <- 1:dim(lprec)[1]

  types <- .Call("RlpSolve_get_constr_type", lprec, as.integer(constraints))
  if(char) types <- c("NA", "<=", ">=", "=")[types+1]
  types
}

