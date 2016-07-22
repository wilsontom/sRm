#' @rdname meta
#'

setMethod("meta", signature = "SRM",
          function(object){

          cat("file id", object@meta$fileID, sep = " : ")
          cat("\n", "\n")
          cat("Instrument", object@meta$instrument_model, sep = " : ")
          cat("\n", "\n")
          if(length(object@meta$precision) == 1){cat("Precision", object@meta$precision, sep = " : ")}

          if(length(object@meta$precision > 1)){
            cat("Precision:", "\n")
            for(i in 1:length(object@meta$precision)){
              cat(object@meta$precision[i], sep = " : ")
              cat("\n")
            }
          }
          cat("\n", "\n")
          cat("Compression", object@meta$compressin, sep = " : ")
          cat("\n", "\n")
          cat("Conversion schema", object@meta$schema, sep = " : ")
          cat("\n", "\n")
       }
)
