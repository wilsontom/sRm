#' SRM
#'
#' A S4 class to store selective reaction monitoring (SRM) mass spectrometry (MS) data
#'
#' @slot env class environment
#' @slot SHA1 character vector of the file SHA1 checksum value
#' @slot meta list containing file meta data
#' @slot totalIonCount \code{data.frame} of sample total ion count (TIC)
#' @slot index character vector of scan index's; ie \code{SRM SIC 341.014,114.098}
#' @slot peaks list of peak data
#' @slot header \code{data.frame} of summary information for each scan index; parentMz, Q3mz, totalIonCount, basePeakInt
#'
setClass(Class = "SRM", representation = representation(
          env = "environment",
          SHA1 = "character",
          meta = "list",
          totalIonCount = "data.frame",
          index = "vector",
          peaks = "list",
          header = "data.frame"
        )
    )

