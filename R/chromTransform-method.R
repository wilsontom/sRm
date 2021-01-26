#' @rdname chromTransform
#'

setMethod('chromTransform', signature = 'SRM',
          function(object, cf)
          {
            chrom_split  <-
              object@rawChrom %>% dplyr::group_by(sampleID, index) %>% dplyr::group_split()

            chrom_basecor <-
              purrr::map(chrom_split, ~ {
                ptw::baseline.corr(.$int)
              })

            for (i in seq_along(chrom_split)) {
              chrom_split[[i]]$int <- chrom_basecor[[i]]
            }

            chrom_smooth <-
              purrr::map(chrom_split, ~ {
                MsCoreUtils::smooth(.$int, cf)
              })

            for (i in seq_along(chrom_split)) {
              chrom_split[[i]]$int <- chrom_smooth[[i]]
            }

            object@transformedChrom <-
              chrom_split %>% dplyr::bind_rows()

            return(object)

          })
