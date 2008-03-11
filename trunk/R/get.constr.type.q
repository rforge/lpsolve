get.constr.type <- function(lprec, constraints = NULL, as.char = TRUE)
{
  if(is.null(constraints))
    constraints <- 1:dim(lprec)[1]

  types <- .Call("RlpSolve_get_constr_type", lprec, as.integer(constraints))
  if(as.char) types <- c("free", "<=", ">=", "=")[types+1]
  types
}

