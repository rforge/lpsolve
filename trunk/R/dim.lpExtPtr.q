dim.lpExtPtr <- function(x)
  c(.Call("RlpSolve_get_Nrows", x, PACKAGE = "lpSolve"),
    .Call("RlpSolve_get_Ncolumns", x, PACKAGE = "lpSolve"))


"dim<-.lpExtPtr" <- function(x, value)
  stop("use the ", sQuote("resize.lp"), " function to set the number of rows ",
       "and columns")

