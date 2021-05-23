#' Plot Sample
#'
#' @rdname plotSample
#' @param object a SRM object
#' @param sampleName a character string of a valid `sampleName`
#' @param polarity a character string of either `pos` or `neg` for positive and negative ionisation mode respectively
#' @return a ggplot plot object
#'
#' @export

setMethod('plotSample', signature = 'SRM',
          function(object, sampleName, polarity) {

            plot_tibble <-
              object@chroms %>% dplyr::filter(sampleID == !!sampleName)

            transition_match <-
              match(plot_tibble$filter, object@header$filter)

            plot_tibble$filter <-
              paste0(object@header$Q1[transition_match], '//', object@header$Q3[transition_match])

              plot_tibble <-
              plot_tibble %>% dplyr::mutate(polarity = object@header$polarity[transition_match])

              if (polarity == 'pos') {
                plot_filter <- plot_tibble %>% dplyr::filter(polarity == '+')
              }

              if (polarity == 'neg') {
                plot_filter <- plot_tibble %>% dplyr::filter(polarity == '-')
              }


              plot_out <- ggplot(data = plot_filter,
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
                facet_wrap(~filter, scales = 'free') +
                theme(strip.text.x = element_text(size = 8))


              if (polarity == 'neg') {
                plot_out <-
                  plot_out + labs(subtitle = paste0(sampleName, ': Negative Mode'))
              }

              if (polarity == 'pos') {
                plot_out <-
                  plot_out + labs(subtitle = paste0(sampleName, ': Positive Mode'))
              }


            return(plot_out)

          })
