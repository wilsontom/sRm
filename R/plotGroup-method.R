#' Plot Group
#'
#' @rdname plotGroup
#' @param object a SRM object
#' @param group a character string of the `GroupID`
#' @return a ggplot plot object
#'
#' @export
#' @importFrom ggplot2 geom_jitter

setMethod('plotGroup', signature = 'SRM',
          function(object, group) {

            if (!'GroupID' %in% names(object@peaks) &
                length(object@groups) == 0) {
              message(crayon::red(
                crayon::bold('!'),
                crayon::yellow('No Peak Group information found')
              ))
              return(invisible(NULL))
            }


            if (!'GroupID' %in% names(object@peaks)) {
              if (group %in% object@groups$GroupID) {
                group_info <- object@groups %>% dplyr::filter(GroupID == group)
              }else{
                message(crayon::red(crayon::bold(cli::symbol$cross),
                                    crayon::yellow('Group:', group, 'not found')))
                return(invisible(NULL))
              }
            }

            if (length(object@groups) == 0) {
              if (!group %in% object@peaks$GroupID) {
                message(crayon::red(crayon::bold(cli::symbol$cross),
                                    crayon::yellow('Group:', group, 'not found')))
                return(invisible(NULL))
              } else{
                group_info <- object@peaks %>% dplyr::filter(GroupID == group)
              }
            }



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


            group_info$rt <- round(group_info$rt, digits = 2)

            group_rt_min <- min(group_info$rt)
            group_rt_max <- max(group_info$rt)

            p_info <-
              ggplot(group_info, aes(x = rt, y = int / max(int))) +
              geom_jitter(
                aes(fill = sampleID),
                shape = 21,
                size = 3.5,
                height = 0.2,
                width = 0
              ) + theme_classic() +
              theme(
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(size = 10, face = "bold")
              ) + labs(
                x = 'Rt (mins)',
                y = 'Relative Intensity',
                subtitle = paste0('Rt width: ', group_rt_min, ' - ', group_rt_max)
              )

            group_plot2 <- patchwork::wrap_plots(p_group, p_info)

            return(group_plot2)

          })
