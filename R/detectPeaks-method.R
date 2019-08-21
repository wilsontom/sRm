#' @rdname detectPeaks
#'

setMethod('detectPeaks', signature = 'SRM',
          function(object, method, ...) {
            arguments <- list(...)


            if (length(object@transformedChrom) == 0) {
              chrom_split  <-
                object@rawChrom %>% group_by(sampleID, index) %>% group_split()
            } else{
              chrom_split  <-
                object@transformedChrom %>% group_by(sampleID, index) %>% group_split()
            }



            if (method == 'pracma') {
              chromPeaks <-
                purrr::map(chrom_split, ~ {
                  pracma_peak_picking(.$rt, .$int)
                })

              for (i in seq_along(chromPeaks)) {
                chromPeaks[[i]] <-
                  chromPeaks[[i]] %>% mutate(sampleID = chrom_split[[i]]$sampleID[1],
                                             index = chrom_split[[i]]$index[1]) %>%
                  select(sampleID, index, rt, rtmin, rtmax, int, area, peakId)

              }



              if (method == 'matchedFilter') {
                chromPeaks <-
                  purrr::map(chrom_split,  ~ {
                    matchedFilter(.$int, .$rt, snthresh = arguments$snthresh)
                  })


                for (i in seq_along(chromPeaks)) {
                  chromPeaks[[i]] <-
                    chromPeaks[[i]] %>% mutate(sampleID = chrom_split[[i]]$sampleID[1],
                                               index = chrom_split[[i]]$index[1]) %>%
                    select(sampleID, index, rt, rtmin, rtmax, int, area, sn, peakId)

                }


              }



              object@peaks <-  chromPeaks %>% bind_rows()

            }

            return(object)

          })
