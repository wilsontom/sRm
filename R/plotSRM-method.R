#' @rdname plotSRM
#' @importFrom ggplot2 ggplot aes_string geom_line theme_bw theme element_blank element_text scale_x_continuous xlab ylab facet_wrap labs ggtitle

setMethod('plotSRM', signature = 'SRM',
          function(object, index, type = 'overlay') {
            plot_tr_name <-
              object@transitions %>% dplyr::filter(index == !!index)

            plot_tibble <-
              object@chroms %>% dplyr::filter(filter == plot_tr_name$filter) %>% dplyr::select(-filter)

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
                theme(legend.title = element_blank()) +
                theme(strip.text.x = element_text(size = 10)) +
                theme(
                  axis.text.y = element_text(size = 10),
                  axis.text.x = element_text(size = 10),
                  axis.title.y = element_text(size = 10),
                  axis.title.x = element_text(size = 10)
                ) +
                scale_x_continuous(breaks = seq(
                  from  = round(min(plot_tibble$rt), digits = 1),
                  to = round(max(plot_tibble$rt), digits = 1),
                  by = 0.5
                )) +
                xlab("Retention Time (mins)") + ylab("Intensity") +
                ggtitle(plot_title) + theme(plot.title = element_text(size = 14))

            }

            if (type == 'facet') {
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
                  from  = round(min(plot_tibble$rt), digits = 1),
                  to = round(max(plot_tibble$rt), digits = 1),
                  by = 0.5
                )) +
                xlab("Retention Time (mins)") + ylab("Intensity") +
                ggtitle(plot_title) + theme(plot.title = element_text(size = 14)) +
                facet_wrap( ~ sampleID)

            }

            return(plot_out)

          })
