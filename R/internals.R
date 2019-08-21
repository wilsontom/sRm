
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
    'transition'
  )
)


#' Local Minima
#'
#' Find all local minima's in a time series vector
#'
#' @param x a numeric vector
#' @return a vector of local minima indicies
#'
#' @keywords internal

local_min <- function(x)
{
  y <- diff(c(.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}


#' Local Maxima
#'
#' Find all local maxima's in a time series vector
#'
#' @param x a numeric vector
#' @return a vector of local maxima indicies
#'
#' @keywords internal

local_max <- function(x)
{
  y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  return(y)
}


#' Estimate Noise (Maxima's)
#'
#' Make an approximate estimation of baseline noise by taking the mean intensity of all local minima's after removing regions
#' where a chromatographic peak may be present (local maxima's)
#'
#' @param x a numeric vector of intensity
#' @return a numeric value
#'
#' @keywords internal

estimate_noise <- function(x)
{
  lmax <- local_max(x)
  lmin <- local_min(x[-lmax])

  #return(as.numeric(median(x[lmin])))
  return(as.numeric(mean(x[lmin])))
}



#' Integrate Peak (pracma)
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @param range a numeric vector of peak indicies
#'
#' @keywords internal

inegrate_peak_pracma <- function(rt, int, range)
{
  # stick some checking in here

  peak_area <- pracma::polyarea(int[range], rt[range])

  return(as.numeric(peak_area))
}


#' Format scan header
#'
#' @param x a `tibble`
#' @return a vector of tidy scan filters
#'
#' @keywords internal

format_scan_header <- function(x)
{
  tidy_header <- NULL
  for (i in seq_along(x$index)) {
    if (x[i, 'index'] == 'TIC') {
      tidy_header[[i]] <- 'TIC'
    } else{
      tidy_header[[i]] <-
        paste0('Q1;', x[i, 'Q1'], ' --> Q3;', x[i, 'Q3'], ' (', x[i, 'polarity'], ')')
    }

  }

  return(tidy_header)
}
