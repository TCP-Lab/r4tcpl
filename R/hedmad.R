# Header Info ------------------------------------------------------------------
#
# cmatools - A collection of utility functions used in our R projects
#
# by //--Luca "Hedmad" Visentin--//
#


#' Combine cat
#'
#' Combine then print to stdout the input variables.
#'
#' This similar to running `cat(paste0(...))`. By default adds a newline
#' at the end of the concatenated string, but it can be overridden through the
#' `end` argument.
#' Gracefully handles printing vectors, by default showing them as comma-separated
#' values, e.g. "`(a , b, c, ...)`".
#'
#' @param ... Values to concatenate then print.
#' @param sep Passed onto `paste0`. Defaults to an empty string (no separation).
#' @param end Character to add to the end of the pasted string. Defaults to `\\n`
#' @param vecsep Separator of vector elements. Defaults to "`, `"
#' @param vecstart Starting character(s) of vector elements. Defaults to "`(`".
#' @param vecend Ending character(s) of vector elements. Defaults to "`)`".
#'
#' @export
ccat <- function(..., sep = "", end = "\n", vecsep = ", ", vecstart = "(", vecend = ")") {
  values <- list(...)

  collapse_vec <- function(item) {
    if (length(item) > 1) {
      return(paste0(vecstart, paste0(item, collapse = vecsep), vecend, sep = ""))
    } else {
      return(item)
    }
  }

  values <- lapply(values, collapse_vec)

  res <- paste(values, collapse = sep)
  res <- paste0(res, end)
  # I do this in a second round to not add "sep" between the last string and "end"

  cat(res)

  return(invisible())
}

