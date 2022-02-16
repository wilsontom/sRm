#' Reduce Peaks
#'
#' @description Reduce detected peaks using a relative area threshold
#' @rdname reducePeaks
#' @param object a SRM object
#' @param relative_area a numeric value between 0 and 1, for the threshold of relative peak area
#' @return a `SRM` object
#'
#' @export

setMethod('reducePeaks', signature = 'SRM',
          function(object, relative_area = 0.05)
          {
            object@peaks <-
              object@peaks %>% dplyr::group_by(sampleID, index) %>% dplyr::mutate(RelArea = (area / max(area))) %>%
              dplyr::ungroup() %>% dplyr::filter(RelArea >= relative_area) %>%
              dplyr::select(-RelArea)


            return(object)


          })
