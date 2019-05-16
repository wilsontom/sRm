#' show-SRM
#' @rdname show
#'
#' @param object a SRM object
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @importFrom methods show

setMethod("show", signature = "SRM",
          function(object) {
            cat('An SRM Object')
            cat('\n')
            cat('--------------')
            cat('\n')
            cat('Number of SRM Files: ', nrow(object@meta))
            cat('\n')
            trcnt <- object@transitions %>% filter(index != 'TIC') %>% nrow()
            cat('Unique SRM Transitons Measured:', trcnt)
            cat('\n')
            cat('Total Object Size:', format(object.size(object), units = 'Mb'))
          })
