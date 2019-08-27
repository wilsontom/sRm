#' @rdname filterPeaks
#'

setMethod('filterPeaks', signature = 'SRM',
          function(object)
          {
            meanScanTime <-
              object@rawChrom %>% dplyr::group_by(sampleID, index) %>%
              dplyr::group_split() %>% purrr::map(., ~ {
                round(.$rt * 60, digits = 0)
              }) %>% purrr::map_dbl(., ~ {
                mean(diff(.))
              }) %>% mean(.)


            peak_widths <-
              object@peaks %>% dplyr::mutate(width = (rtmax - rtmin) * 60)

            five_points_width <- 5 * meanScanTime

            object@peaks <-
              object@peaks %>% dplyr::filter(width < five_points_width)

            return(object)

          })
