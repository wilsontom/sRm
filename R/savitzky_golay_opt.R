#' (Optimised) Savitzky Golay
#'
#' @param rt a numeric vector of retention time
#' @param int a numeric vector of intensity
#' @param ford a numeric value for the filter order (2 = quadratic filter, 4 = quartic)
#' @return a numeric vector of smoothed intensity
#' @export

savitzky_golay_opt <- function(rt, int, ford)
{
  f <- seq(from = 3, to = 101, by = 2)

  filtered_int <- NULL
  for (i in seq_along(f)) {
    filtered_int[[i]] <- pracma::savgol(int, f[i], forder = ford, dorder = 0)
  }

  min_filt_in <-
    purrr::map_dbl(filtered_int, min) %>% tibble::tibble(x = f, y = .)


  min_ratio <- (1 - min_filt_in$y / min_filt_in$x)
  min_diff <- diff(min_ratio)

  f_min <- which(min_diff == min(min_diff))

  smoothed_int <-
    pracma::savgol(int,
                   fl = f[f_min+1],
                   forder = ford,
                   dorder = 0)

  smoothed_int[smoothed_int < 0] <- 0

  return(smoothed_int)

}
