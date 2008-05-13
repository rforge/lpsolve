guess.basis <- function(lprec, guess)
{
  if(length(guess) != dim(lprec)[2])
    stop("the length of ", sQuote("guess"), " must be the same as the number",
         " of columns and constraints in the model")

  basis <- .Call("RlpSolve_guess_basis", lprec, as.double(guess),
                  PACKAGE = "lpSolveAPI")

  if(basis[1] == 1)
    basis <- basis[-1]
  else
    basis <- NULL

  basis
}


