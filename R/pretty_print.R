#'------------------------------------------------------------------------------
#' @description To align strings in console as if using Word tab stops.
#'
#' @param spaces Tab width.
#' @param including String to be included in space count.
#'
#' @returns A string consisting of the proper number of space characters.
#'
#' @author //FeAR//
#'------------------------------------------------------------------------------
tab <- function(spaces = 5, including = "") {

  if (spaces < nchar(including)) {
    stop("Word to include exceeds tab spaces... can't align!")
  }

  whiteSpace <- paste(rep(" ", spaces-nchar(including)), collapse = "")
  return(whiteSpace)
}
