#' Plot Group
#'
#' @rdname plotGroup
#' @param object a SRM object
#' @param group a character string of the `GroupID`
#' @return a ggplot plot object
#'
#' @export

setMethod('plotGroup', signature = 'SRM',
          function(object, group,){


          group_info <- object@peaks %>% dplyr::filter(GroupID == !!group)

          peak_area_plots <-
            purrr::map2(group_info$sampleID, group_info$index,
                        ~ {
                          plotPeakArea(object, index = .y, sampleName = .x)
                        })









          }
)
