#' Keep Transitions
#' @rdname keepTransitions
#' @param object a SRM object
#' @param index_keep a numeric vector of transition index to retain
#' @return a `SRM` object
#'
#' @export

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
