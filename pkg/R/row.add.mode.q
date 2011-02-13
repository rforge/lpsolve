row.add.mode <- function(lprec, state)
{
  if(!missing(state)) {
    state <- match.arg(state, choices = c("off", "on")) == "on"
    .Call("RlpSolve_set_add_rowmode", lprec, as.logical(state),
           PACKAGE = "lpSolveAPI")
  }

  ifelse(.Call("RlpSolve_is_add_rowmode", lprec, PACKAGE = "lpSolveAPI"),
         "on", "off")
}


