#' Pracma Peak-Picking
#'
#' A wrapper around `pracma::findPeaks` for peak detection and integration
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#'
#' @return a `tibble` of peak infomation (rt, rtmin, rtmax, int, area, peakId)
#' @export

pracma_peak_picking <- function(rt, int)
{
  noise <- estimate_noise(int)

  peak_indicies <- pracma::findpeaks(int, threshold = noise)

  if (is.null(peak_indicies)) {
    peak_indicies <- pracma::findpeaks(int, threshold = 0)
  }


  peak_indicies <- data.frame(peak_indicies)
  names(peak_indicies) <- c('height', 'apex', 'left', 'right')


  peak_id <- seq(from = 1, to = nrow(peak_indicies))

  rtv <- rt[peak_indicies$apex]
  rtmin <- rt[peak_indicies$left]
  rtmax <- rt[peak_indicies$right]
  intv <- int[peak_indicies$apex]


  pkranges <- list()
  for (i in seq_along(peak_indicies$left)) {
    pkranges[[i]] <- peak_indicies$left[i]:peak_indicies$right[i]
  }

  peak_area <- list()
  for (i in seq_along(pkranges)) {
    peak_area[[i]] <-
      pracma::polyarea(int[pkranges[[i]]], rt[pkranges[[i]]])
  }


  pkarea <- unlist(peak_area)


  peak_info_tibble <-
    tibble::tibble(
      rt = rtv,
      rtmin = rtmin,
      rtmax = rtmax,
      int = intv,
      area = pkarea,
      peakId = peak_id
    )


  return(peak_info_tibble)


}
