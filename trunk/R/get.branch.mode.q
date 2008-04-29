get.branch.mode <- function(lprec, columns = 1:n, as.char = TRUE)
{
  n <- dim(lprec)[2]

  modes <- .Call("RlpSolve_get_var_branch", lprec, as.integer(columns),
                  PACKAGE = "lpSolve")

  if(as.char)
    modes <- c("ceiling", "floor", "auto", "default")[1 + modes]

  modes
}


