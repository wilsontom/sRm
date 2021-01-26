#' @rdname detectPeaks
#'

setMethod('detectPeaks', signature = 'SRM',
          function(object,
                   raw = TRUE,
                   snthresh = 10,
                   fwhm = 30) {
            if (isTRUE(raw)) {
              chrom_split  <-
                object@rawChrom %>% dplyr::group_by(sampleID, filter) %>% dplyr::group_split()
            } else{
              chrom_split  <-
                object@transformedChrom %>% dplyr::group_by(sampleID, filter) %>% dplyr::group_split()
            }


            chromPeaks <-
              purrr::map(chrom_split,  ~ {
                matchedFilter(.$rt, .$int, snthresh = snthresh, fwhm = fwhm)
              })

            for (i in seq_along(chromPeaks)) {
              chromPeaks[[i]] <-
                chromPeaks[[i]] %>% dplyr::mutate(sampleID = chrom_split[[i]]$sampleID[1],
                                                  filter = chrom_split[[i]]$filter[1]) %>%
                dplyr::select(sampleID, filter, rt, rtmin, rtmax, int, area, sn, peakId)
            }

            object@peaks <-
              chromPeaks %>% dplyr::bind_rows() %>% dplyr::filter(peakId != 0)

            return(object)

          })
