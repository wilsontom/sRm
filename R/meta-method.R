#' @rdname meta
#'

setMethod("meta", signature = "SRM",
          function(object){
          cat("\n")
          cat("File ID : ", object@meta$file_id)
          cat("\n", "\n")
          cat("Instrument Model : ", object@meta$instrument_model)
          cat("\n", "\n")
          cat("mzML Schema", object@meta$mzml_schema, sep = " : ")
          cat("\n", "\n")
          cat("Acquisition Date", object@meta$acquisition_date, sep = " : ")
          cat("\n", "\n")
          cat("Acquisition Time", object@meta$acquisition_time, sep = " : ")
       }
)
