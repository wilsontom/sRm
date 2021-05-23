#' Detect Peaks
#'
#' @rdname detectPeaks
#' @param object a SRM object
#' @param snthresh a numeric value for the signal-to-noise threshold to use
#' @param peakwidth a numeric vector indicating the minimum and maximum tolerated peak width
#'
#' @return a SRM object
#'
#' @export

setMethod('detectPeaks', signature = 'SRM',
          function(object,
                   snthresh = 10,
                   peakwidth = 30)
            {

              chrom_split  <-
                object@chroms %>% dplyr::group_by(sampleID, filter) %>% dplyr::group_split()


            chromPeaks <-
              purrr::map(chrom_split,  ~ {
                centWave(.$rt, .$int, snthresh = snthresh, peakwidth = peakwidth)
              })


                         for (i in seq_along(chromPeaks)) {
              chromPeaks[[i]] <-
                chromPeaks[[i]] %>% dplyr::mutate(sampleID = chrom_split[[i]]$sampleID[1],
                                                  filter = chrom_split[[i]]$filter[1]) %>%
                dplyr::select(sampleID, filter, rt, rtmin, rtmax, int, area, sn, peakId)
            }

            object@peaks <-
              chromPeaks %>% dplyr::bind_rows() %>% dplyr::filter(peakId != 0) %>%
              dplyr::left_join(., object@transitions, by = 'filter')

            return(object)

          })
