#' Plot Group
#'
#' @rdname plotGroup
#' @param object a SRM object
#' @param group a character string of the `GroupID`
#' @return a ggplot plot object
#'
#' @export

setMethod('plotGroup', signature = 'SRM',
          function(object, group) {
            group_info <- object@peaks %>% dplyr::filter(GroupID == group)

            peak_indicies <- list()
            for (i in 1:nrow(group_info)) {
              chrom_tibble <-
                object@chroms %>% dplyr::filter(filter == group_info$filter[i] &
                                                  sampleID == group_info$sampleID[i]) %>%
                dplyr::select(-filter)

              left_idx <-
                which(chrom_tibble$rt == group_info$rtmin[i])
              right_idx <-
                which(chrom_tibble$rt == group_info$rtmax[i])

              peak_indicies[[i]] <-
                tibble::tibble(
                  rt = chrom_tibble$rt[left_idx:right_idx],
                  int = chrom_tibble$int[left_idx:right_idx],
                  SampleID = rep(group_info$sampleID[i])
                )

            }

            peak_group_all <- peak_indicies %>% dplyr::bind_rows()

            p_group <-
              ggplot(peak_group_all,
                     aes(
                       x = rt,
                       y = int,
                       group = SampleID,
                       colour = SampleID
                     )) +
              geom_line(size = 0.45) + theme_classic() +
              theme(
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(size = 10, face = "bold")
              ) + labs(
                x = "Rt (mins)",
                y = "Intensity",
                title = paste0('Group: ', group),
                subtitle = unique(group_info$transition)
              )


            return(p_group)

          })
