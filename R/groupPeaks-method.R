#' Group Peaks
#'
#' @description Group detected peaks based on retention time. Based on `MsCoreUtils::group`
#' @rdname groupPeaks
#' @param object a SRM object
#' @param rt_tolerance a numeric value for the tolerate retention grouping
#' @return a `SRM` object
#'
#' @export

setMethod('groupPeaks', signature = 'SRM',
          function(object, rt_tolerance)
          {
            peak_groups <-
              object@peaks %>% dplyr::group_by(filter) %>% dplyr::group_split()

            rt_groups <-
              peak_groups %>% purrr::map(., ~ {
                groupPeaksInternal(.$rt, rt_tolerance = rt_tolerance)
              })

            object@peaks <- purrr::map2(peak_groups, rt_groups, ~ {
              dplyr::mutate(.x, group = .y)
            }) %>% dplyr::bind_rows()


            groupid_tmp <-
              object@peaks %>% dplyr::select(index, group) %>% unique()

            GroupN <- seq(from = 1,
                          to = nrow(groupid_tmp),
                          by = 1)

            pad_length <- nchar(max(GroupN))

            GroupN <-
              stringr::str_pad(GroupN, pad = 0, width = pad_length)

            GroupID <- stringr::str_c('G', GroupN)

            groupid_tmp <-
              groupid_tmp %>% dplyr::mutate(GroupID = GroupID)

            object@peaks <-
              object@peaks %>% dplyr::left_join(., groupid_tmp, by = c('index', 'group')) %>%
              dplyr::select(-group)


            return(object)

          })
