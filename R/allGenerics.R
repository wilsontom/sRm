#' Combine Transitions
#' @rdname combineTransitions
#'
#' @param object a SRM object
#' @return a \code{transition} class of SRM total ion chromatograms
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(name = "combineTransitions",
           def = function(object)
           {standardGeneric("combineTransitions")}
)


#' Show meta data
#' @rdname meta
#'
#' @param object a SRM object
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(name = "meta",
           def = function(object)
           {standardGeneric("meta")}
)

#' Show transitions
#' @rdname transitions
#'
#' @param object a SRM object
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(name = "transitions",
           def = function(object)
           {standardGeneric("transitions")}
)



#' Plot SRM
#' @rdname plotSRM
#' @param object a SRM object
#' @param idn the index number of a transition to plot
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

setGeneric(name = "plotSRM",
           def = function(object,idn)
           {standardGeneric("plotSRM")}
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


setGeneric(name = "plotMulti",
           def = function(object,idn, addLabels = FALSE,labels = NULL)
           {standardGeneric("plotMulti")}
)
