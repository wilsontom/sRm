#' @rdname plotTransform
setMethod('plotTransform', signature = 'SRM',
          function(object, index, sampleID) {

            plot_tr_name <-
              object@transitions %>% dplyr::filter(index_n == !!index_n)

            raw_tibble <-
              object@rawChrom %>% dplyr::filter(index == plot_tr_name$index & sampleID == !!sampleID) %>% dplyr::select(-index) %>%
              dplyr::mutate(Type = 'Raw')

            transform_tibble <-
              object@transformedChrom %>% dplyr::filter(index == plot_tr_name$index & sampleID == !!sampleID) %>% dplyr::select(-index) %>%
              dplyr::mutate(Type = 'Transformed')


            plot_tibble <- dplyr::bind_rows(raw_tibble, transform_tibble)


            plot_title <- plot_tr_name$transition


              plot_out <- ggplot(data = plot_tibble,
                                 aes_string(
                                   x = 'rt',
                                   y = 'int',
                                   group = 'Type',
                                   colour = 'Type'
                                 )) + geom_line(size = 0.6) + theme_bw() +
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
                ggtitle(plot_title) + theme(plot.title = element_text(size = 14))


            return(plot_out)

          })
