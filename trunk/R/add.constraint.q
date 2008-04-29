add.constraint <- function(lprec, xt, type = c("<=", "=", ">="), b,
                           indices = 1:n)
{
  n <- dim(lprec)[2]

  if(length(xt) != length(indices))
    stop(sQuote("xt"), " and ", sQuote("indices"), " are not the same length")

  if(is.character(type)) {
    type <- match.arg(type)
    type <- match(type, c("<=", ">=", "="))
  }

  invisible(.Call("RlpSolve_add_constraintex", lprec, as.double(xt),
                   as.integer(indices), as.integer(type), as.double(b),
                   PACKAGE = "lpSolve"))
}


