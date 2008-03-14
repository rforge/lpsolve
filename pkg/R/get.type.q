get.type <- function(lprec, columns = NULL, as.char = TRUE)
{
  if(is.null(columns))
    columns <- 1:dim(lprec)[2]

  ind <- .Call("RlpSolve_is_int", lprec, as.integer(columns),
                PACKAGE = "lpSolve")

  if(as.char) {
    types <- rep("real", length(columns))
    types[ind] <- "integer"
  }

  else
    types <- ind

  types
}

