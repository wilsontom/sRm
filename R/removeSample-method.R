#' Remove Sample
#' @rdname removeSample
#' @param object a SRM object
#' @param sampleName a character string of a `sampleName` to remove
#' @return a `SRM` object
#'
#' @export

setMethod('removeSample', signature = 'SRM',
          function(object, sampleName)
          {

            object@header <- object@header %>%
              dplyr::filter(sampleID != !!sampleName)

            object@chroms <- object@chroms %>%
              dplyr::filter(sampleID != !!sampleName)

            return(object)


          })
