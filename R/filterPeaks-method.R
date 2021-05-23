#' Filter Peaks
#'
#' Filter detected peaks based on a series of heuristic measures
#'
#' @rdname filterPeaks
#' @param object a SRM object
#' @return a `SRM` object
#'
#' @export

setMethod('filterPeaks', signature = 'SRM',
          function(object)
          {
            meanScanTime <-
              object@chroms %>% dplyr::group_by(sampleID, index) %>%
              dplyr::group_split() %>% purrr::map(., ~ {
                round(.$rt * 60, digits = 0)
              }) %>% purrr::map_dbl(., ~ {
                mean(diff(.))
              }) %>% mean(.)


            peak_widths <-
              object@peaks %>% dplyr::mutate(width = (rtmax - rtmin) * 60)

            # make this a method input option
            five_points_width <- 5 * meanScanTime

            object@peaks <-
              peak_widths %>% dplyr::filter(width > five_points_width)

            # this needs speading up - add as extra method in peakDetection
            rtApex <- list()
            for (i in 1:nrow(object@peaks)) {
              rawChrom <-
                object@rawChrom %>% dplyr::filter(
                  sampleID == object@peaks$sampleID[i] &
                    index == object@peaks$index[i] &
                    rt >= object@peaks$rtmin[i] &
                    rt <= object@peaks$rtmax[i]
                )


              rtApex[[i]] <-
                rawChrom$rt[rawChrom$int == max(rawChrom$int)]
            }
            object@peaks <-
              object@peaks %>% dplyr::mutate(rtApex = unlist(rtApex))


            object@peaks <-
              object@peaks %>% dplyr::filter(rtApex != rtmin & rtApex != rtmax)


            return(object)

          })
