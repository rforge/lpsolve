write.lp <- function(lprec, filename, type = c("lp", "mps", "freemps"))
{
  type <- match.arg(type)
  switch(type,
    "lp" = .Call("RlpSolve_write_lp", lprec, as.character(filename)),
    "mps" = .Call("RlpSolve_write_mps", lprec, as.character(filename)),
    "freemps" = .Call("RlpSolve_write_freemps", lprec, as.character(filename))
  )
}


