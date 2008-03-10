get.branch.mode <- function(lprec, columns = NULL, as.char = TRUE)
{
  if(is.null(columns))
    columns <- 1:dim(lprec)[2]

  modes <- .Call("RlpSolve_get_var_branch", lprec, as.integer(columns))

  if(as.char)
    modes <- c("ceiling", "floor", "auto", "default")[1 + modes]

  modes
}

