#' @rdname transitions
#'

setMethod("transitions", signature = "SRM",
          function(object){

          for(i in seq_along(object@index)){
            cat(object@index[i], "\n")
          }
      }
  )
