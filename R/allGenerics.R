#' Combine Transitions
#' @rdname combineTransitions
#'
#' @param object a SRM object
#' @return a list of SRM total ion chromatograms
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
