#' Combine Transitions
#' @rdname combineTransitions
#'
#' @param object a SRM object
#' @return a \code{transition} class of SRM total ion chromatograms
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = "combineTransitions",
  def = function(object)
  {
    standardGeneric("combineTransitions")
  }
)


#' Show meta data
#' @rdname meta
#'
#' @param object a SRM object
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = "meta",
  def = function(object)
  {
    standardGeneric("meta")
  }
)

#' Show transitions
#' @rdname transitions
#'
#' @param object a SRM object
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = "transitions",
  def = function(object)
  {
    standardGeneric("transitions")
  }
)



#' Plot SRM
#' @rdname plotSRM
#' @param object a SRM object
#' @param idn the index number of a transition to plot
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = "plotSRM",
  def = function(object, idn)
  {
    standardGeneric("plotSRM")
  }
)


#' plotMulti
#' @rdname plotMulti
#' @param object a SRM object
#' @param idn the index number of a transition to plot
#' @param addLabels logical; If \code{TRUE} then labels for each \code{geom_line} are added
#' @param labels an optional character vector of label names. Default is \code{NULL}. If \code{addLabels} is \code{TRUE} and \code{labels} is \code{NULL} then \code{object@index} is used for label text.
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export


setGeneric(
  name = "plotMulti",
  def = function(object,
                 idn,
                 addLabels = FALSE,
                 labels = NULL)
  {
    standardGeneric("plotMulti")
  }
)


#' export_to_skyline
#' @rdname export_to_skyline
#' @param object a SRM object
#' @param transitions an optional \code{data.frame} containing the following columns for selected (or all) transitions.
#' @return a \code{tibble} of transitions in a format which can be easily used with \code{Skyline}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'export_to_skyline',
  def = function(object, transitions = NULL)
  {
    standardGeneric('export_to_skyline')
  }
)


#' tic
#' @rdname tic
#' @param object a SRM object
#' @return a numeric value for the whole sample total ion count (tic)
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'tic',
  def = function(object)
  {
    standardGeneric('tic')
  }
)


#' bpi
#' @rdname bpi
#' @param object a SRM object
#' @return a numeric value for the whole sample base peak intensity (bpi)
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(
  name = 'bpi',
  def = function(object)
  {
    standardGeneric('bpi')
  }
)

#' get
#' @rdname get
#' @param object a SRM object
#' @param Q1 a numeric value for parent m/z
#' @param Q3 a numeric value for product m/z
#' @param method either 'tic' or 'bpi' to return total ion count or base peak intensity respectively
#' @return a \code{tibble} or parent and product mass, and either tic or bpi

setGeneric(
  name = 'get',
  def = function(object, Q1, Q3, method)
  {
    standardGeneric('get')
  }
)





