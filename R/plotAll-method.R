#' Plot all
#'
#' Plot the SRM-MS TIC and all individual Q3 traces for a selected parent scans'
#'
#' @rdname plotAll
#' @param x a \code{transition} class
#' @param n the number of the transition (index) to plot
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

plotAll <- function(x,n){
  UseMethod("plotAll")
}

plotAll.default <- function(x,n)
  {
  cat("S3 method for plotting combined and individual SRM-MS transitions")
  }
