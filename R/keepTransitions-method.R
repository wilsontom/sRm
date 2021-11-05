#' @rdname keepTransitions
#'

setMethod('keepTransitions', signature = 'SRM',
          function(object, index_keep = c())
          {
            object@transitions <- object@transitions %>%
              dplyr::filter(index %in% index_keep)

            object@chroms <- object@chroms %>%
              dplyr::filter(filter %in% object@transitions$filter)

            object@header <- object@header %>%
              dplyr::filter(filter %in% object@transitions$filter)

            return(object)


          })
