set.basis <- function(lprec, basis, nonbasic = FALSE, default = FALSE)
{
  if(default)
    return(invisible(.Call("RlpSolve_default_basis", lprec,
                            PACKAGE = "lpSolveAPI")))

  if(nonbasic)
    if(length(basis) != sum(dim(lprec)))
      stop("the length of ", sQuote("basis"), " must be the same as the number",
           " of columns and constraints in the model")

  else
    if(length(basis) != dim(lprec)[2])
      stop("the length of ", sQuote("basis"), " must be the same as the number",
           " of columns in the model")

  invisible(.Call("RlpSolve_set_basis", lprec, as.integer(c(0, basis)),
                   as.logical(nonbasic), PACKAGE = "lpSolveAPI"))
}


