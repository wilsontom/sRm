#' @rdname meta
#'

setMethod("meta", signature = "SRM",
          function(object){
          cat("\n")
          cat("File ID : ", object@meta$fileID)
          cat("\n", "\n")
          cat("Instrument : ", object@meta$instrument)
          cat("\n", "\n")
          cat("Instrument Model : ", object@meta$instrument_model)
          cat("\n", "\n")
          cat("Instrument Serial Number : ", object@meta$instrument_serial)
          cat("\n", "\n")
          if(length(object@meta$precision) == 1){cat("Precision", object@meta$precision, sep = " : ")}

          if(length(object@meta$precision > 1)){
            cat("Precision:", "\n")
            for(i in 1:length(object@meta$precision)){
              cat(object@meta$precision[i], sep = " : ")
              cat("\n")
            }
          }
          cat("\n")
          cat("Compression", object@meta$compressin, sep = " : ")
          cat("\n", "\n")
          cat("Conversion Schema", object@meta$schema, sep = " : ")
          cat("\n", "\n")
       }
)
