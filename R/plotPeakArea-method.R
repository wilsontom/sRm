#' Plot Peak Area
#'
#' @rdname plotPeakArea
#' @param object a SRM object
#' @param index a numeric value of the transition index to plot
#' @param sampleName a character string of `sampleName` to plot
#' @return a ggplot plot object
#'
#' @export
#' @importFrom ggplot2 geom_polygon guides

setMethod('plotPeakArea', signature = 'SRM',
          function(object, index, sampleName){

            plot_tr_name <-
              object@transitions %>% dplyr::filter(index == !!index)


              chrom_tibble <-
                object@chroms %>% dplyr::filter(filter == plot_tr_name$filter &
                                                    sampleID == !!sampleName) %>% dplyr::select(-filter)




              peak_tibble <-
                object@peaks %>% dplyr::filter(filter == plot_tr_name$filter &
                                                 sampleID == !!sampleName)


            chrom_plot <- ggplot(data = chrom_tibble, aes(x = rt, y = int)) + geom_line(size = 0.45) +
              theme_classic() + xlab("Rt (mins)") + ylab("Intensity") +
              theme(
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(size = 10, face = "bold")
              ) + ggrepel::geom_label_repel(min.segment.length = unit(0, 'lines'),
                                            nudge_y = 1,
                data = subset(chrom_tibble, rt %in% peak_tibble$rt),
                                            aes(label = round(rt, digits = 2)),
                                                size = 3.5,
                                                box.padding = unit(0.1, "lines"),
                                                max.overlaps = 30)



            polygon_indicies <- list()
            for (i in 1:nrow(peak_tibble)) {
              left_idx <- which.min(abs(chrom_tibble$rt - peak_tibble$rtmin[i]))
              right_idx <-
                which.min(abs(chrom_tibble$rt - peak_tibble$rtmax[i]))


              polygon_indicies[[i]] <-
                tibble::tibble(
                  rt = chrom_tibble$rt[left_idx:right_idx],
                  int = chrom_tibble$int[left_idx:right_idx],
                  cls = rep(peak_tibble$peakId[i])
                )

            }

            polygon_all <- polygon_indicies %>% dplyr::bind_rows()
            polygon_all$cls <- factor(polygon_all$cls)

            plot_title <- plot_tr_name$transition

            area_plot <-
              chrom_plot + geom_polygon(data = polygon_all, aes_string(x = 'rt', y = 'int', fill = 'cls')) +
              geom_line(size = 0.5) + guides(fill = "none") +
              labs(
                x = 'Rt (mins)',
                y = 'Relative Intensity',
                subtitle = paste0(sampleName, ": ", plot_title)
              )



            return(area_plot)

          }
)
