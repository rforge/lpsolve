get.kind <- function(lprec, columns = 1:n)
{
  n <- dim(lprec)[2]

  if(n < 1)
    columns <- integer(0)

  ans <- rep("standard", length(columns))

  idx <- .Call("RlpSolve_is_semicont", lprec, as.integer(columns),
                PACKAGE = "lpSolveAPI")
  ans[idx] <- "semi-continuous"

  idx <- .Call("RlpSolve_is_SOS_var", lprec, as.integer(columns),
                PACKAGE = "lpSolveAPI")
  ans[idx] <- "SOS"

  ans
}


