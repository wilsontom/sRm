#' show-SRM
#' @rdname show
#'
#' @param object a SRM object
#' @return NULL
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @importFrom methods show

setMethod("show", signature = "SRM",
          function(object){
            cat("\n")
            cat("File ID : ", object@meta$fileID)
            cat("\n", "\n")
            cat("Acquisition Date : ", object@meta$acqDate)
            cat("\n", "\n")
            cat("Total Acquisition Time : ", round(max(object@totalIonCount$rt), digits = 2), "mins")
            cat("\n", "\n")
            cat("-----Transition Data-----")
            cat("\n")
            cat(nrow(object@header), "SRM Transitions measured")
            cat("\n")
            cat(length(unique(object@header$parent)), "Unique Q1 (parent) Ions")
            cat("\n")
            cat(length(unique(object@header$product)), "Unique Q3 (product) Ions")
          }
)
