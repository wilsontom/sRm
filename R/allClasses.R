#' SRM
#'
#' A S4 class to store single reaction monitoring (SRM) mass spectrometry (MS) data

#' @slot meta a `tibble` of a minimal amount of meta data.
#' @slot transitions a `tibble` of all transitions names
#' @slot chroms a `tibble` of retention time (rt) and intensity (int) values for all imported SRM chromatograms
#' @slot peaks a `tibble` of detected peaks
#' @slot header a `tibble` of formatted header data from `mzR::header()`

setClass(Class = 'SRM', representation = representation(
          meta = 'tbl_df',
          transitions = 'tbl_df',
          chroms = 'tbl_df',
          peaks = 'tbl_df',
          header = 'tbl_df'
        )
    )

