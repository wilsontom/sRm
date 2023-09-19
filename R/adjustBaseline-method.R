#' Adjust Baseline
#'
#' @rdname adjustBaseline
#' @param object a SRM object
#' @param hwm a numeric value for window half width for local medians
#' @return a SRM object
#'
#' @export
#
setMethod('adjustBaseline', signature = 'SRM',
          function(object, hwm = 6)
          {
            chrom_split  <-
              object@chroms %>% dplyr::group_by(sampleID, filter) %>% dplyr::group_split()



            baseline_adjuster <- function(x, hwm = hwm)
            {
              x <-
                baseline::baseline(as.matrix(t(x)), method = 'medianWindow', hwm = hwm)
              xcor <- as.vector(t(x@corrected))
              xcor[xcor < 0] <- 0
              return(xcor)
            }


            chrom_base <-
              purrr::map(chrom_split, ~ {
                baseline_adjuster(.$int, hwm)
              })

            for (i in seq_along(chrom_split)) {
              chrom_split[[i]]$int <- chrom_base[[i]]
            }

            object@chroms <-
              chrom_split %>% dplyr::bind_rows()

            return(object)

          })
