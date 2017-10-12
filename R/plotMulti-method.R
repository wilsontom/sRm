#' @rdname plotMulti
#' @importFrom ggplot2 guides theme_classic

setMethod("plotMulti", signature = "SRM",
          function(object,
                   idn,
                   addLabels = TRUE,
                   labels = NULL) {
            idn_names <- object@index[idn]
            idn_peaks <- object@peaks[idn]

            for (i in seq_along(idn_peaks)) {
              if (is.null(labels)) {
                idn_peaks[[i]] <-
                  tibble::add_column(idn_peaks[[i]], name = rep(idn_names[[i]], nrow(idn_peaks[[i]])))
              }
              if (!is.null(labels)) {
                idn_peaks[[i]] <-
                  tibble::add_column(idn_peaks[[i]], name = rep(labels[i], nrow(idn_peaks[[i]])))
              }

            }

            peaks_all <- data.frame(do.call('rbind', idn_peaks))

            peaks_all[, 'name'] <-
              factor(peaks_all[, 'name'], levels = unique(peaks_all[, 'name']))

            if (addLabels == FALSE) {
              plot_multi <-
                ggplot(data = peaks_all,
                       aes_string(
                         x = 'rt',
                         y = 'int',
                         group = 'name',
                         colour = 'name'
                       )) +
                geom_line() + theme_bw() +
                guides(group = "none") +
                guides(colour = "none")  +
                xlab("Retention Time (mins)") +
                ylab("Intensity") +
                theme(
                  axis.text.y = element_text(size = 11, face = "bold"),
                  axis.text.x = element_text(size = 11, face = "bold"),
                  axis.title.y = element_text(size = 11, face = "bold"),
                  axis.title.x = element_text(size = 11, face = "bold")
                )
              plot(plot_multi)
            }


            if (addLabels == TRUE) {
              label_df <-
                data.frame(matrix(ncol = 3, nrow = length(idn)))
              names(label_df) <- c('rt', 'int', 'name')


              for (i in seq_along(idn_peaks)) {
                maxint <-
                  which(idn_peaks[[i]][, 'int'] == max(idn_peaks[[i]][, 'int']))
                label_df[i, 'rt'] <- idn_peaks[[i]][maxint, 'rt']
                label_df[i, 'int'] <- idn_peaks[[i]][maxint, 'int']

                if (is.null(labels)) {
                  label_df[i, 'name'] <- unique(idn_peaks[[i]][, 'name'])

                } else{
                  label_df[i, 'name'] <- labels[i]

                }

              }
              label_df[, 'name'] <- factor(label_df[, 'name'])

              plot_multi <-
                ggplot(data = peaks_all,
                       aes_string(
                         x = 'rt',
                         y = 'int',
                         group = 'name',
                         colour = 'name'
                       )) +
                geom_line() + theme_bw()  + ggrepel::geom_label_repel(data = label_df, aes_string(x = 'rt', y = 'int', label = 'name')) +
                guides(group = "none") +
                guides(colour = "none")  +
                xlab("Retention Time (mins)") +
                ylab("Intensity") +
                theme(
                  axis.text.y = element_text(size = 11, face = "bold"),
                  axis.text.x = element_text(size = 11, face = "bold"),
                  axis.title.y = element_text(size = 11, face = "bold"),
                  axis.title.x = element_text(size = 11, face = "bold")
                )

              plot(plot_multi)
            }

          })
