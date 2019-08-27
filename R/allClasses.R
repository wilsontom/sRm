#' SRM
#'
#' A S4 class to store single reaction monitoring (SRM) mass spectrometry (MS) data


setClass(Class = 'SRM', representation = representation(
          meta = 'tbl_df',
          transitions = 'tbl_df',
          rawChrom = 'tbl_df',
          transformedChrom = 'tbl_df',
          peaks = 'tbl_df',
          header = 'tbl_df',
          processingLog = 'list'
        )
    )

