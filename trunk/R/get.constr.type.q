get.constr.type <- function(lprec, constraints = 1:m, as.char = TRUE)
{
  m <- dim(lprec)[1]
  types <- .Call("RlpSolve_get_constr_type", lprec, as.integer(constraints),
                  PACKAGE = "lpSolve")
  if(as.char) types <- c("free", "<=", ">=", "=")[types+1]
  types
}


