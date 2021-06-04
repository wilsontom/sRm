#' Create Group
#'
#' @description Create peak group based on user specified retention time
#' @rdname createGroup
#' @param object a SRM object
#' @param rt a numeric value of target Rt time (in minutes)
#' @param index a numeric value of the target transition index
#' @param width a numeric value of tolerated Rt width (in seconds)
#' @param id a character string for peak group identification
#' @return a `SRM` object
#'
#' @export

setMethod('createGroup', signature = 'SRM',
          function(object, index, rt, width, id)
          {

            rtmin <- rt - (width / 60)
            rtmax <- rt + (width  / 60)

            peak_extract <-
              object@peaks %>% dplyr::filter(index == !!index) %>%
              dplyr::filter(rt <= !!rtmax & rt >= !!rtmin) %>%
              dplyr::mutate(GroupID = !!id)


            if (length(object@groups) == 0) {
              object@groups <- peak_extract
            } else{
              object@groups <- object@groups %>% dplyr::bind_rows(., peak_extract)
            }

            return(object)

          })
