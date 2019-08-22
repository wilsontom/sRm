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
  def = function(object, index, type)
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
#' @param sampleName a character string of a valid `sampleName`
#' @param polarity a character string of either `pos` or `neg` for positive and negative ionisation mode respectively
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
#' @rdname plotCompareSample
#' @param object a SRM object
#' @param index a numeric value of the transition index to plot
#' @param sampleName a character vector of `sampleName` to plot
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

#' Smooth Chromatograms
#'
#' @rdname smoothChrom
#' @param object a SRM object
#' @param method a character string indicating the smoothing method to use. `sgolay` for optimised Savitzky Golay smoothing.
#' @param ... any additional parameters that are required by the smoothing method
#' @return a SRM object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'smoothChrom',
  def = function(object, method, ...)
  {
    standardGeneric('smoothChrom')
  }
)


#' Detect Peaks
#'
#' @rdname detectPeaks
#' @param object a SRM object
#' @param method a character string indicating the peak detection method to use:
#' - **pracma**
#' - **matchedFilter**
#'
#' @param ... any additional parameters that are required by the peak detection method
#' @return a SRM object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'detectPeaks',
  def = function(object, method, ...)
  {
    standardGeneric('detectPeaks')
  }
)

#' Plot Peak Area
#'
#' @rdname plotPeakArea
#' @param object a SRM object
#' @param index a numeric value of the transition index to plot
#' @param sampleName a character string of `sampleName` to plot
#' @param type a character string of either `raw` or `transformed`, indicating which chromatography data to use when plotting
#' @return a ggplot plot object
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'plotPeakArea',
  def = function(object, index, sampleName, type)
  {
    standardGeneric('plotPeakArea')
  }
)
