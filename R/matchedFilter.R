#' Wraper for xcms matchedFilter peak detection
#'
#'
#' @param rt a numeric vector of chromatogram retention time in minutes
#' @param int a numeric vector of chromatogram intensity
#' @param snthresh a numeric value for the Signal-to-Noise cut off to use during peak detection
#' @return a `tibble` of detected peaks
#'
#' @export

matchedFilter <- function(rt, int, snthresh)
{
  MFpeaks <-
    suppressWarnings(xcms::peaksWithMatchedFilter(
      int = int,
      rt = rt * 60,
      snthresh = snthresh
    )) %>% tibble::as_tibble()

  if (nrow(MFpeaks) > 0) {
    peakTable <-
      MFpeaks %>% dplyr::select(rt, rtmin, rtmax, maxo, into, sn) %>% dplyr::mutate(peakId = seq(from = 1, to = nrow(.))) %>%
      dplyr::mutate(rt = rt / 60,
                    rtmin = rtmin / 60,
                    rtmax = rtmax / 60) %>%
      dplyr::rename(int = maxo, area = into)


  }

  if (nrow(MFpeaks) == 0) {
    peakTable <-
      MFpeaks %>% dplyr::select(rt, rtmin, rtmax, maxo, into, sn) %>% dplyr::mutate(peakId = 0) %>%
      dplyr::rename(int = maxo, area = into)

    peakTable[1,] <- 0

  }

  return(peakTable)

}
