#' @rdname bpi
#'

setMethod('bpi', signature = 'SRM',
          function(object){
            max(object@totIonCount[,'int'])
          }
)

