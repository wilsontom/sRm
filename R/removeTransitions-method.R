#' Keep Transitions
#' @rdname removeTransitions
#' @param object a SRM object
#' @param index_keep a numeric vector of transition index to remove
#' @return a `SRM` object
#'
#' @export

setMethod('removeTransitions', signature = 'SRM',
          function(object, index_out = c())
          {
            object@transitions <- object@transitions %>%
              dplyr::filter(!index %in% index_out)

            object@chroms <- object@chroms %>%
              dplyr::filter(!filter %in% object@transitions$filter)

            object@header <- object@header %>%
              dplyr::filter(!filter %in% object@transitions$filter)

            return(object)


          })
