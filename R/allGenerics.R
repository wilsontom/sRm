#' Plot SRM
#'
#' @rdname plotSRM
#' @param object a SRM object
#' @param index a numeric value of the transition index to plot
#' @param type a character string of either `overlay` or `facet`
#' @return a ggplot plot object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = "plotSRM",
  def = function(object, index, type = 'overlay')
  {
    standardGeneric("plotSRM")
  }
)
