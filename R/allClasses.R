#' SRM
#'
#' A S4 class to store single reaction monitoring (SRM) mass spectrometry (MS) data
#'
#' @slot SHA1 character vector of the original SHA-1 checksum value
#' @slot meta list containing file meta data
#' @slot totIonCount \code{data.frame} of sample total ion count (TIC)
#' @slot filter character vector of scan filters; taken directly from \code{chromatogram idRefs}
#' @slot index character vector of readable scan index's; ie \code{SRM SIC 341.014,114.098}
#' @slot peaks list of peak data
#' @slot header \code{data.frame} of summary information for each scan index; \code{parentMz, Q3mz, totalIonCount, basePeakInt}
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
