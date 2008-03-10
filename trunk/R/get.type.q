get.type <- function(lprec, columns = NULL, char = TRUE)
{
  if(is.null(columns))
    columns <- 1:dim(lprec)[2]

  ans <- .Call("RlpSolve_is_int", lprec, as.integer(columns))

  if(char) {
    ans <- rep("real", length(columns))
    ans[.Call("RlpSolve_is_int", lprec, as.integer(columns))] <- "integer"
  }

  else
    ans <- .Call("RlpSolve_is_int", lprec, as.integer(columns))

  ans
}

