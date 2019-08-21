#' @rdname plotCompareSample
#'

setMethod('plotCompareSample', signature = 'SRM',
          function(object, index, sampleName) {

            plot_tr_name <- object@transitions %>% dplyr::filter(index_n == !!index)

            plot_tibble <-
              object@rawChrom %>% dplyr::filter(index == plot_tr_name$index) %>% dplyr::select(-index) %>% dplyr::filter(sampleID %in% !!sampleName)

            plot_title <- plot_tr_name$transition

            plot_out <- ggplot(data = plot_tibble,
                                 aes_string(x = 'rt',
                                            y = 'int')) + geom_line(size = 0.45) + theme_bw() +
                theme(legend.position = 'top') +
                theme(legend.title = element_blank()) +
                theme(strip.text.x = element_text(size = 10)) +
                theme(
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10),
                  axis.title.y = element_text(size = 10),
                  axis.title.x = element_text(size = 10)
                ) +
                scale_x_continuous(breaks = seq(
                  from  = 0,
                  to = round(max(plot_tibble$rt), digits = 1),
                  by = 2
                )) +
                xlab("Retention Time (mins)") + ylab("Intensity") +
                ggtitle(plot_title) + theme(plot.title = element_text(size = 14)) +
                facet_wrap( ~ sampleID, ncol = 1)


            return(plot_out)

          })
