#' Group Summary
#'
#' Create a summary table for peak groups
#' @rdname groupSummary
#' @param object a `SRM` object
#' @return a `tibble` of group summary
#' @export


setMethod('groupSummary', signature = 'SRM',
          function(object)
          {
            group_summary <-
              object@peaks %>% dplyr::group_by(transition,  GroupID) %>%
              dplyr::summarise(
                Rt = median(rt),
                Rtmin = min(rtmin),
                Rtmax = max(rtmax),
                SN = mean(sn),
                count = dplyr::n()
              )

            return(group_summary)
          })
