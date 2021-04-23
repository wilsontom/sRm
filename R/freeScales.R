#' Free Scales
#'
#' Allow `free` scales in facet plots
#'
#' @param ggplot a `ggplot` object
#' @return a `ggplot` object
#' @export

freeScales <- function(ggplot)
{
  ggplot$facet$params$free$x <- TRUE
  ggplot$facet$params$free$y <- TRUE
  return(ggplot)
}
