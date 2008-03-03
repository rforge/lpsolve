get.types <- function(lprec, columns = NULL, char = TRUE)
{
  n <- dim(lprec)[2]

  if(is.null(columns))
    columns <- 1:n

  ans <- .Call("RlpSolve_is_int", lprec, as.integer(columns))

  if(char) {
    ans <- rep("real", length(columns))
    ans[.Call("RlpSolve_is_int", lprec, as.integer(columns))] <- "integer"
  }

  else
    ans <- .Call("RlpSolve_is_int", lprec, as.integer(columns))

  ans
}

