#' SRM
#'
#' A S4 class to store single reaction monitoring (SRM) mass spectrometry (MS) data
#'
#' @slot SHA1 character vector of the original SHA-1 checksum value
#' @slot meta list containing file meta data
#' @slot totIonCount \code{data.frame} of sample total ion count (TIC)
#' @slot filter character vector of scan filters; taken directly from \code{chromatogram idRefs}
#' @slot index character vector of readable scan index's; ie \code{Q1: 153.01 --> Q3: 65.271 (-)}
#' @slot peaks list of peak data
#' @slot header \code{data.frame} of summary information for each scan index;\code{parent}, \code{product}, \code{polarity}, \code{totIonCount}, \code{basePeakInt}
#'
setClass(Class = "SRM", representation = representation(
          SHA1 = "character",
          meta = "list",
          totIonCount = "data.frame",
          filter = "vector",
          index = "vector",
          peaks = "list",
          header = "data.frame"
        )
    )
