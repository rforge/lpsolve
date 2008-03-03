read.lp <- function(filename, type = c("lp", "mps", "freemps"))
{
  if(!file.exists(filename))
    stop(dQuote(filename), ": no such file")

  type <- match.arg(type)
  lp <- switch(type,
    "lp" = .Call("RlpSolve_read_LP", as.character(filename)),
    "mps" = .Call("RlpSolve_read_MPS", as.character(filename)),
    "freemps" = .Call("RlpSolve_read_freeMPS", as.character(filename))
  )

  if(is.null(lp))
    stop("could not interpret ", basename(filename), " as an ", type, " file")

  oldClass(lp) <- "lpExtPtr"
  lp
}


