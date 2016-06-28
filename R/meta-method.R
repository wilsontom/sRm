#' #' Show meta data
#' @rdname meta
#'

setMethod("meta", signature = "SRM",
          function(object){

          cat("file id", object@meta$fileID, sep = " : ")
          cat("\n", "\n")
          cat("Instrument", object@meta$instrument_model, sep = " : ")
          cat("\n", "\n")
          cat("Precision", object@meta$precision, sep = " : ")
          cat("\n", "\n")
          cat("Compression", object@meta$compressin, sep = " : ")
          cat("\n", "\n")
          cat("Conversion scheme", object@meta$schema, sep = " : ")
          cat("\n", "\n")
       }
)
