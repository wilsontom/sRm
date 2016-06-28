#' SRM
#'
#' A S4 class to store selective reaction monitoring (SRM) mass spectrometry (MS) data
#'
#' @slot env
#' @slot SHA1
#' @slot meta
#' @slot totalIonCount
#' @slot index
#' @slot peaks
#' @slot header
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

