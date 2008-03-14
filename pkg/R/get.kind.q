get.kind <- function(lprec, columns = NULL)
{
  if(is.null(columns))
    columns <- 1:dim(lprec)[2]

  ans <- rep("standard", length(columns))

  idx <- .Call("RlpSolve_is_semicont", lprec, as.integer(columns),
                PACKAGE = "lpSolve")
  ans[idx] <- "semi-continuous"

  idx <- .Call("RlpSolve_is_SOS_var", lprec, as.integer(columns),
                PACKAGE = "lpSolve")
  ans[idx] <- "SOS"

  ans
}

