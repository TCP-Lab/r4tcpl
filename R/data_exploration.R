
#' Show a snippet of the dataset
#'
#' This function is superseeded by the `print` implementation of a `tibble`.
#' It is included here as an example function.
#'
#' @param dataset A `data.frame` to show
#' @param name The name of the dataset, used to enrich the output.
#' @param rows The number of rows to show.
#' @param cols The number of cols to show.
#'
#' @return A dimesion vector of the form `c(rows, cols)`.
#' @export
#'
#' @examples
#' x <- data.frame(var1 = c(1, 2, 3, 4, 5), var2 = c(6, 7, 8, 9, 10))
#' show.data(x, "an example dataset", rows = 3)
show.data <- function(dataset, name = NULL, rows = 10, cols = 5)
{
  d = dim(dataset)
  cat("\nDataset", name, "dimensions:", d[1], "x", d[2], "\n\n", sep = " ")

  rows = min(d[1],rows)
  cols = min(d[2],cols)

  # 'print' because automatic printing is turned off in loops (and functions)
  print(dataset[1:rows,1:cols])

  return(d)
}
