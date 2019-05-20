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
  name = 'plotSRM',
  def = function(object, index, type = 'overlay')
  {
    standardGeneric('plotSRM')
  }
)


#' Plot Parent
#'
#' @rdname plotParent
#' @param object a SRM object
#' @param parentMass a numeric value of the parent mass (Q1) to extract
#' @return a ggplot plot object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'plotParent',
  def = function(object, parentMass)
  {
    standardGeneric('plotParent')
  }
)


#' Plot Sample
#'
#' @rdname plotSample
#' @param object a SRM object
#' @param sampleName
#' @param polarity
#' @return a ggplot plot object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export


setGeneric(
  name = 'plotSample',
  def = function(object, sampleName, polarity)
  {
    standardGeneric('plotSample')
  }
)



#' Plot Compare Sample
#'
#' @rdname plotSample
#' @param object a SRM object
#' @param index
#' @param sampleName
#' @return a ggplot plot object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export


setGeneric(
  name = 'plotCompareSample',
  def = function(object, index, sampleName)
  {
    standardGeneric('plotCompareSample')
  }
)

