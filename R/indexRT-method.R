#' Find Target Retention Time (Rt) for a transition
#'
#' @rdname indexRT
#' @param object a SRM object
#' @param index a numeric value for the transition index
#' @param sampleName a character string
#' @param n a numeric value for the number of Rts to return (if > 1 is available)
#' @return a numeric vector or Rt targets
#'
#' @export

setMethod('indexRT', signature = 'SRM',
          function(object, index, sampleName, n = 1) {
            plot_tr_name <-
              object@transitions %>% dplyr::filter(index == !!index)

            chrom_tibble <-
              object@chroms %>% dplyr::filter(filter == plot_tr_name$filter &
                                                sampleID == !!sampleName) %>% dplyr::select(-filter)

            local_max_rt <-
              round(chrom_tibble$rt[MsCoreUtils::localMaxima(chrom_tibble$int, hws = 10)], digits = 2)

            local_max_int <-
              round(chrom_tibble$int[MsCoreUtils::localMaxima(chrom_tibble$int, hws = 10)], digits = 2) /
              sum(chrom_tibble$int) * 100

            local_max_tib <- tibble::tibble(rt = local_max_rt, int = local_max_int) %>%
              dplyr::filter(int >= 1.0) %>% dplyr::arrange(-int)

              return(local_max_tib$rt[1:n])

          })
