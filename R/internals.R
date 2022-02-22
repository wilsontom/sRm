
globalVariables(
  c(
    '.',
    'accession',
    'polarity',
    'PrecursorCharge',
    'PrecursorMz',
    'PrecursorName',
    'PrecursorRT',
    'ProductCharge',
    'ProductMz',
    'name',
    'parent',
    'product',
    'rt',
    'int',
    'totIonCount',
    'basePeakInt',
    'rtmin',
    'rtmax',
    'maxo',
    'into',
    'sn',
    'index',
    'peakId',
    'index_n',
    'Q1',
    'sampleID',
    'area',
    'Datestamp',
    'Instrument',
    'sample_n',
    'Schema',
    'Timestamp',
    'transition',
    'filter',
    'width',
    'GroupID',
    'SampleID',
    'Rtmax',
    'Rtmin',
    'group'
  )
)


#' Format scan header
#'
#' @param x a `tibble`
#' @return a vector of tidy scan filters
#'
#' @keywords internal

format_scan_header <- function(x)
{
  tidy_header <- NULL
  for (i in seq_along(x$filter)) {
    if (x[i, 'filter'] == 'TIC') {
      tidy_header[[i]] <- 'TIC'
    } else{
      tidy_header[[i]] <-
        paste0(x[i, 'Q1'], ' > ', x[i, 'Q3'], ' (', x[i, 'polarity'], ')')
    }

  }

  return(tidy_header)
}


#' Group Peaks
#'
#' Group detected peaks based on retention time. Based on `MsCoreUtils::group`
#'
#' @param x a numeric vector of retention times
#' @param rt_tolerance a numeric value for the tolerate retention grouping
#' @return a numeric vector of retention time groups
#'
#' @keywords internal

groupPeaksInternal <- function(x, rt_tolerance) {
  if (is.unsorted(x)) {
    idx <- order(x)
    x <- x[idx]
  } else
    idx <- integer()
  rt_tolerance <- rt_tolerance + sqrt(.Machine$double.eps)
  res <- cumsum(c(1L, diff(x) >= rt_tolerance))
  res[idx] <- res
  res
}



#' Wraper for xcms centWave peak detection
#'
#'
#' @param rt a numeric vector of chromatogram retention time in minutes
#' @param int a numeric vector of chromatogram intensity
#' @param snthresh a numeric value for the Signal-to-Noise cut off to use during peak detection
#' @param peakwidth a numeric value for the minimum and maximum tolerated peak width.
#' @return a `tibble` of detected peaks
#'
#' @keywords internal

centWave <- function(rt, int, snthresh, peakwidth)
{
  CWpeaks <-
    suppressWarnings(
      xcms::peaksWithCentWave(
        int = int,
        rt = rt * 60,
        snthresh = snthresh,
        peakwidth = peakwidth,
        prefilter = c(1, 5),
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


