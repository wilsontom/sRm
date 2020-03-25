#' @rdname smoothChrom
#'

setMethod('smoothChrom', signature = 'SRM',
          function(object, method, ...) {
            arguments <- list(...)

            chrom_split  <-
              object@rawChrom %>% dplyr::group_by(sampleID, index) %>% dplyr::group_split()

            if (method == 'sgolay') {
              smoothed_chrom <- chrom_split %>% purrr::map(., ~ {
                savitzky_golay_opt(.$rt, .$int, ford = as.numeric(arguments$ford))
              })
            }


            for (i in seq_along(chrom_split)) {
              chrom_split[[i]]$int <- smoothed_chrom[[i]]
            }

            object@transformedChrom <-
              chrom_split %>% dplyr::bind_rows()

            return(object)

          })
