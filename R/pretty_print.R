
tab <- function(spaces = 5, including = "") {

  if (spaces < nchar(including)) {
    stop("Word to include exceeds tab spaces... can't align!")
  }

  whiteSpace <- paste(rep(" ", spaces-nchar(including)), collapse = "")
  return(whiteSpace)
}
