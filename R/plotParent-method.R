#' @rdname plotParent
#'

setMethod('plotParent', signature = 'SRM',
          function(object, parentMass) {
            plot_tr_name <-
              object@header %>% dplyr::filter(Q1 == !!parentMass) %>% dplyr::select(filter) %>% dplyr::distinct()

            plot_tibble <-
              object@chroms %>% dplyr::filter(filter %in% plot_tr_name$filter)

            transition_match <-
              match(plot_tibble$filter, object@header$filter)

            plot_tibble$filter <-
              object@header$transition[transition_match]



            plot_out <- ggplot(data = plot_tibble,
                               aes_string(
                                 x = 'rt',
                                 y = 'int',
                                 group = 'filter',
                                 colour = 'filter'
                               )) + geom_line(size = 0.50) + theme_bw() +
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
              facet_wrap(~ sampleID)

            return(plot_out)

          })
