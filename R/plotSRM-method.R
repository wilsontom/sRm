#' @rdname plotSRM
#'
setMethod("plotSRM", signature = "SRM",
            function(object,idn){

          plot_df <- data.frame(object@peaks[idn])
          names(plot_df) <- c("rt", "int")

          plot_title <- as.character(object@index[idn])
          plot_title <- gsub("SRM SIC", "", plot_title)
          plot_title <- gsub(",", " : Q3 ", plot_title)
          plot_title <- paste("Q1", plot_title, sep = "")

          ggplot(data = plot_df, aes_string(x = 'rt', y = 'int')) + geom_line(size = 0.5) + theme_bw() +
            theme(legend.position = "none") +
            theme(strip.text.x = element_text(size=12, face="bold")) +
            theme(axis.text.y = element_text(size = 12, face = "bold"),
                  axis.text.x = element_text(size = 12, face = "bold"),
                  axis.title.y = element_text(size = 12, face = "bold"),
                  axis.title.x = element_text(size = 12, face = "bold")) +
                   xlab("Retention Time (mins)") + ylab("Intensity") +
                   ggtitle(plot_title) + theme(plot.title=element_text(size = 14, face = "bold"))

            }
)
