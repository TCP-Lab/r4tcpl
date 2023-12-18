# Header Info ------------------------------------------------------------------
# 
# r4tcpl - A collection of utility functions used in our R projects
#
# Special "ion tools" for Biophysics class
# by //--FeA.R--//
#



#' Current Plot
#' @export
#' @import graphics stats
#' 
#' @description A multi-trace viewer for single-channel ion currents. Starting
#'              from a data set containing many current traces, `iTrace()` makes
#'              a composite plot showing the first three single-channel traces
#'              and a fourth subplot with the total current. This is meant to
#'              highlight the transition from the stochastic behavior of the
#'              single ion channel to the deterministic electrical current
#'              observed at the whole-cell level.
#'              
#' @param dataset A data frame or a matrix containing a different single-channel
#'                current trace **per row**.
#'
#' @author FeA.R
iTrace <- function(dataset) {
  # Set graphical parameters
  opar <- par(no.readonly = TRUE) # Make a copy of the original settings
  par(cex = 0.7, mai = c(0.1, 0.7, 0.2, 0.2))
  plot.new()
  
  for (j in 1:3) {
    par(fig = c(0, 1, 0.8-0.2*(j-1), 1-0.2*(j-1)), new = TRUE)
    # Plot as time series
    plot(as.ts(dataset[j,]), xlab = "", ylab = paste("channel", j), xaxt = 'n')
  }
  total_curent <- colSums(dataset)
  par(fig = c(0, 1, 0.1, 0.4), new = TRUE)
  plot(as.ts(total_curent), xlab = "time", ylab = "Total current", col = "red")
  
  # restore original settings
  par(opar)
}



#' Current Map
#' @export
#' @import graphics grDevices
#' 
#' @description This function provides a comprehensive graphical representation
#'              of many single-channel ion currents as a 2D colormap.
#'              
#' @param dataset A data frame or a matrix containing a different single-channel
#'                current trace **per row**.
#' 
#' @author FeA.R
iMap <- function(dataset)
{
  image(rotate(dataset, 1),
        col = hcl.colors(12, "RdPu", rev = TRUE),
        xlab = "time", ylab = "single channel current")
}

