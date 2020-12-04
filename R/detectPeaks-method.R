#' @rdname detectPeaks
#'

setMethod('detectPeaks', signature = 'SRM',
          function(object, method, ...) {
            arguments <- list(...)


            if (length(object@transformedChrom) > 0) {
              chrom_split  <-
                object@transformedChrom %>% dplyr::group_by(sampleID, index) %>% dplyr::group_split()
            } else{
              chrom_split  <-
                object@rawChrom %>% dplyr::group_by(sampleID, index) %>% dplyr::group_split()
            }



            if (method == 'pracma') {
              chromPeaks <-
                purrr::map(chrom_split, ~ {
                  pracma_peak_picking(.$rt, .$int)
                })

              for (i in seq_along(chromPeaks)) {
                chromPeaks[[i]] <-
                  chromPeaks[[i]] %>% dplyr::mutate(sampleID = chrom_split[[i]]$sampleID[1],
                                                    index = chrom_split[[i]]$index[1]) %>%
                  dplyr::select(sampleID, index, rt, rtmin, rtmax, int, area, peakId)

              }
            }



            if (method == 'matchedFilter') {
              chromPeaks <-
                purrr::map(chrom_split,  ~ {
                  matchedFilter(.$rt, .$int, snthresh = arguments$snthresh, fwhm = arguments$fwhm)
                })

                for (i in seq_along(chromPeaks)) {
                chromPeaks[[i]] <-
                  chromPeaks[[i]] %>% dplyr::mutate(sampleID = chrom_split[[i]]$sampleID[1],
                                                    index = chrom_split[[i]]$index[1]) %>%
                  dplyr::select(sampleID, index, rt, rtmin, rtmax, int, area, sn, peakId)

              }

            }

            object@peaks <- chromPeaks %>% dplyr::bind_rows() %>% dplyr::filter(peakId != 0)

            return(object)

          })
