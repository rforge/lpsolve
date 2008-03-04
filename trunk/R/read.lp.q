read.lp <- function(filename, type = c("lp", "mps", "freemps"))
{
  if(!file.exists(filename))
    stop(dQuote(filename), ": no such file")

  if(missing(type)) {
    type <- strsplit(basename(filename), split = ".", fixed = TRUE)[[1]]
    type <- casefold(type[length(type)])
    type <- match(type, c("lp", "mps", "freemps"), nomatch = -1)
    if(type < 0)
      stop("unable to determine file type - please use the ", sQuote("type"),
           " argument")
    else
      type <- c("lp", "mps", "freemps")[type]
  }

  else
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


