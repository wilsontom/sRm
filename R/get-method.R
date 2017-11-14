#' @rdname get
#'

setMethod('get', signature = 'SRM',
          function(object, Q1, Q3, method) {
            if (is.null(Q3)) {
              filter_res <- filter(object@header, parent == Q1)
            } else{
              filter_res <-
                filter(object@header, parent ==  Q1 & product == Q3)
            }

            if (method == 'tic') {
              get_res <-
                filter_res %>% select(., parent, product, tic = totIonCount)
            }
            if (method == 'bpi') {
              get_res <-
                filter_res %>% select(., parent, product, bpi =  basePeakInt)
            }
            return(get_res)
          })
