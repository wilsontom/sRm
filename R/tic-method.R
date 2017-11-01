#' @rdname tic
#'

setMethod('tic', signature = 'SRM',
          function(object){
            sum(object@totIonCount[,'int'])
          }
)
