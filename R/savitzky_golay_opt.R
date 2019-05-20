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
    filtered_int[[i]] <- pracma::savgol(int, f[i], forder = ford)
  }

  noise <- estimate_noise(int)

  min_int <-
    which(unlist(purrr::map(filtered_int, ~ {
      abs(min(.)) > noise
    })) == FALSE)

  selected_f <- f[max(min_int)]
  #selected_f <- 11
  smoothed_int <-
    pracma::savgol(int,
                   fl = selected_f,
                   forder = ford,
                   dorder = 0)

  smoothed_int[smoothed_int < 0] <- 0

  return(smoothed_int)

}
