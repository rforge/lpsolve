dim.default <- base::dim

dim.lpExtPtr <- function(x)
  c(.Call("RlpSolve_get_Nrows", x), .Call("RlpSolve_get_Ncolumns", x))

dim <- function(x)
  UseMethod("dim")

