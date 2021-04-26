#' Wraper for xcms centWave peak detection
#'
#'
#' @param rt a numeric vector of chromatogram retention time in minutes
#' @param int a numeric vector of chromatogram intensity
#' @param snthresh a numeric value for the Signal-to-Noise cut off to use during peak detection
#' @param peakwidth a numeric value for the minimum and maximum tolerated peak width.
#' @return a `tibble` of detected peaks
#'
#' @export

centWave <- function(rt, int, snthresh, peakwidth)
{
  CWpeaks <-
    suppressWarnings(
      xcms::peaksWithCentWave(
        int = int,
        rt = rt * 60,
        snthresh = snthresh,
        peakwidth = peakwidth,
        integrate = 2
      )
    ) %>% tibble::as_tibble()

  if (nrow(CWpeaks) > 0) {
    peakTable <-
      CWpeaks %>% dplyr::select(rt, rtmin, rtmax, maxo, into, sn) %>% dplyr::mutate(peakId = seq(from = 1, to = nrow(.))) %>%
      dplyr::mutate(rt = rt / 60,
                    rtmin = rtmin / 60,
                    rtmax = rtmax / 60) %>%
      dplyr::rename(int = maxo, area = into)


  }

  if (nrow(CWpeaks) == 0) {
    peakTable <-
      CWpeaks %>% dplyr::select(rt, rtmin, rtmax, maxo, into, sn) %>% dplyr::mutate(peakId = 0) %>%
      dplyr::rename(int = maxo, area = into)

    peakTable[1, ] <- 0

  }

  return(peakTable)

}
