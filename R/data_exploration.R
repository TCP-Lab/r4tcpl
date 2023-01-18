#'------------------------------------------------------------------------------
#' @description Print the upper leftmost corner of the data set (typically an
#'              expression matrix) also showing its full dimensions and
#'              controlling for out-of-bounds exceptions.
#'
#' @param dataset Matrix or data frame to print.
#' @param rows Maximum number of rows to display.
#' @param cols Maximum number of columns to display.
#'
#' @returns A vector containing <dataset> dimensions.
#'
#' @author //FeAR//
#'------------------------------------------------------------------------------
show.data = function(dataset, name = NULL, rows = 10, cols = 5)
{
  d = dim(dataset)
  cat("\nDataset", name, "dimensions:", d[1], "x", d[2], "\n\n", sep = " ")

  rows = min(d[1],rows)
  cols = min(d[2],cols)

  # 'print' because automatic printing is turned off in loops (and functions)
  print(dataset[1:rows,1:cols])

  return(d)
}
