#' Accessor Methods for the SRM S4 class
#'
#' @include allClasses.R
#' @include allGenerics.R
#'
#' transitions
#' @rdname transitions
#' @description List all transitions
#' @param object a `SRM` S4 object
#' @export

setMethod('transitions', signature = 'SRM',
          function(object) {
            object@transitions
          })

#' header
#' @rdname header
#' @description List object header information
#' @param object a `SRM` S4 object
#' @export

setMethod('header', signature = 'SRM',
          function(object) {
            object@header
          })



#' peaks
#' @rdname peaks
#' @description List detected peaks
#' @param object a `SRM` S4 object
#' @export

setMethod('peaks', signature = 'SRM',
          function(object) {
            object@peaks
          })



#' meta
#' @rdname meta
#' @description List sample meta data
#' @param object a `SRM` S4 object
#' @export

setMethod('meta', signature = 'SRM',
          function(object) {
            object@meta
          })





