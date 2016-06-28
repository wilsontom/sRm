#' #' Show trasitions
#' @rdname transitions
#'

setMethod("transitions", signature = "SRM",
          function(object){

          trans_ob <- combineTransitions(object)
          for(i in 1:length(trans_ob)){
            cat(names(trans_ob)[[i]], "\n")
          }
      }
  )
