#' @rdname plotPeakArea
#' @importFrom ggplot2 geom_polygon guides


setMethod('plotPeakArea', signature = 'SRM',
          function(object, index, sampleName){

            plot_tr_name <-
              object@transitions %>% dplyr::filter(index == !!index)


              chrom_tibble <-
                object@rawChrom %>% dplyr::filter(filter == plot_tr_name$filter &
                                                    sampleID == !!sampleName) %>% dplyr::select(-filter)


            chrom_plot <-
              ggplot(data = chrom_tibble, aes(x = rt, y = int)) + geom_line(size = 0.45) +
              theme_classic() + xlab("Rt (mins)") + ylab("Intensity") +
              theme(
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                axis.title.y = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(size = 10, face = "bold")
              )



            peak_tibble <-
              object@peaks %>% dplyr::filter(filter == plot_tr_name$filter &
                                               sampleID == !!sampleName)


            polygon_indicies <- list()
            for (i in 1:nrow(peak_tibble)) {
              left_idx <- which(chrom_tibble$rt == peak_tibble$rtmin[i])
              right_idx <-
                which(chrom_tibble$rt == peak_tibble$rtmax[i])

              polygon_indicies[[i]] <-
                tibble::tibble(
                  rt = chrom_tibble$rt[left_idx:right_idx],
                  int = chrom_tibble$int[left_idx:right_idx],
                  cls = rep(peak_tibble$peakId[i])
                )

            }

            polygon_all <- polygon_indicies %>% dplyr::bind_rows()
            polygon_all$cls <- factor(polygon_all$cls)

            area_plot <-
              chrom_plot + geom_polygon(data = polygon_all, aes_string(x = 'rt', y = 'int', fill = 'cls')) +
              geom_line(size = 0.5) + guides(fill = "none")



            return(area_plot)

          }
)
