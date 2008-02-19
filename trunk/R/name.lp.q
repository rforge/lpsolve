name.lp <- function(lprec)
  .Call("RlpSolve_get_lp_name", x)


"name.lp<-" <- function(lprec, value) {
  .Call("RlpSolve_set_lp_name", x, as.character(value[1]))
  lprec
}

