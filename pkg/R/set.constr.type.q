set.constr.type <- function(lprec, types, indices = NULL)
{
  m <- dim(lprec)[1]

  if(is.null(indices)) {
    if(length(types) != m)
      stop(sQuote("types"), " must contain one relation for each constraint",
           " in the model")
    indices <- 1:m
  }

  else
    if(length(indices) != length(types))
      stop(sQuote("types"), " and ", sQuote("indices"), " must contain the",
           " same number of elements")

  if(is.character(types))
    types <- match(types, c("<=", ">=", "="), nomatch = 0)

  invisible(.Call("RlpSolve_set_constr_type", lprec, as.integer(indices),
                   as.integer(types), PACKAGE = "lpSolve"))
}

