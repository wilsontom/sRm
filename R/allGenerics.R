
#' @rdname plotSRM

setGeneric(
  name = 'plotSRM',
  def = function(object, index, type = 'overlay')
  {
    standardGeneric('plotSRM')
  }
)

#' @rdname plotParent

setGeneric(
  name = 'plotParent',
  def = function(object, parentMass)
  {
    standardGeneric('plotParent')
  }
)

#' @rdname plotSample


setGeneric(
  name = 'plotSample',
  def = function(object, sampleName, polarity)
  {
    standardGeneric('plotSample')
  }
)


#' @rdname chromTransform

setGeneric(
  name = 'chromTransform',
  def = function(object, cf)
  {
    standardGeneric('chromTransform')
  }
)


#' @rdname detectPeaks

setGeneric(
  name = 'detectPeaks',
  def = function(object, snthresh, peakwidth)
  {
    standardGeneric('detectPeaks')
  }
)


#' @rdname plotPeakArea

setGeneric(
  name = 'plotPeakArea',
  def = function(object, index, sampleName)
  {
    standardGeneric('plotPeakArea')
  }
)


#' @rdname filterPeaks

setGeneric(
  name = 'filterPeaks',
  def = function(object)
  {
    standardGeneric('filterPeaks')
  }
)

#' @rdname peakAsymmetry

setGeneric(
  name = 'peakAsymmetry',
  def = function(object)
  {
    standardGeneric('peakAsymmetry')
  }
)


#' @rdname groupPeaks

setGeneric(
  name = 'groupPeaks',
  def = function(object, rt_tolerance)
  {
    standardGeneric('groupPeaks')
  }
)


#' @rdname transitions

setGeneric(
  name = 'transitions',
  def = function(object)
  {
    standardGeneric('transitions')
  }
)

#' @rdname header

setGeneric(
  name = 'header',
  def = function(object)
  {
    standardGeneric('header')
  }
)

#' @rdname peaks

setGeneric(
  name = 'peaks',
  def = function(object)
  {
    standardGeneric('peaks')
  }
)


#' @rdname meta

setGeneric(
  name = 'meta',
  def = function(object)
  {
    standardGeneric('meta')
  }
)

#' @rdname groupSummary

setGeneric(
  name = 'groupSummary',
  def = function(object)
  {
    standardGeneric('groupSummary')
  }
)


#' @rdname plotGroup

setGeneric(
  name = 'plotGroup',
  def = function(object, group)
  {
    standardGeneric('plotGroup')
  }
)


#' @rdname createGroup

setGeneric(
  name = 'createGroup',
  def = function(object,index, rt, width, id)
  {
    standardGeneric('createGroup')
  }
)


#' @rdname indexRT

setGeneric(
  name = 'indexRT',
  def = function(object,index, sampleName, n)
  {
    standardGeneric('indexRT')
  }
)

















#' Keep Transitions
#'
#' @rdname keepTransitions
#' @param object a SRM object
#' @param index_keep a numeric vector of transition index to retain
#' @return a `SRM` object
#'
#' @export

setGeneric(
  name = 'keepTransitions',
  def = function(object, index_keep)
  {
    standardGeneric('keepTransitions')
  }
)
