
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
    'width'
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


