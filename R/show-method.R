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
            cat("Filename : ", object@meta$filename)
            cat("\n", "\n")
            cat("Acquisition Date : ", object@meta$acquisition_date)
            cat("\n", "\n")
            cat("Total Acquisition Time : ", round(max(object@totIonCount$rt), digits = 2), "mins")
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
