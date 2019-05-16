#' @rdname plotSRM
#'

setMethod('plotSRM', signature = 'SRM',
          function(object, index, type = 'overlay') {
            plot_tr_name <- object@transitions %>% filter(index_n == !!index)

            plot_tibble <-
              object@rawChrom %>% filter(index == plot_tr_name$index) %>% select(-index)

            plot_title <- plot_tr_name$transition

            if (type == 'overlay') {
              plot_out <- ggplot(data = plot_tibble,
                                 aes_string(
                                   x = 'rt',
                                   y = 'int',
                                   group = 'sampleID',
                                   colour = 'sampleID'
                                 )) + geom_line(size = 0.45) + theme_bw() +
                theme(legend.position = 'top') +
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
                ggtitle(plot_title) + theme(plot.title = element_text(size = 14))

            }

            if (type == 'facet') {
              plot_out <- ggplot(data = plot_tibble,
                                 aes_string(x = 'rt',
                                            y = 'int')) + geom_line(size = 0.45) + theme_bw() +
                theme(legend.position = 'top') +
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
                facet_wrap( ~ sampleID)

            }

            return(plot_out)

          })
