print.lpExtPtr <- function(x, ...)
{
  .Call("RlpSolve_print_lp", x)
  invisible(x)
}


