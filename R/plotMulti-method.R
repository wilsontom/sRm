#' @rdname plotMulti
#' @importFrom ggplot2 guides

setMethod("plotMulti", signature = "SRM",
          function(object,idn, labels = NULL){

          idn_names <- object@index[idn]
          idn_peaks <- object@peaks[idn]

          idn_list <- lapply(idn_peaks, function(x)(idn_list <- data.frame(rt = x$rt, int = x$int)))

          for(i in seq_along(idn_list)){
            nm_tmp <- rep(names(idn_list)[i], nrow(idn_list[[i]]))
            idn_list[[i]] <- data.frame(idn_list[[i]], name = nm_tmp)
          }

          idn_df <- do.call("rbind", idn_list)
          idn_df[,"name"] <- as.character(idn_df[,"name"])
          idn_df[,"name"] <- gsub("SRM SIC ", "", idn_df[,"name"])


          label_list <- lapply(idn_list, function(x)(which(x$int == max(x$int))))
          label_df <- data.frame(matrix(nrow = length(label_list), ncol = 3))

          for(i in seq_along(label_list)){
            label_df[i,1] <- idn_list[[i]][label_list[[i]],"rt"]
            label_df[i,2] <- idn_list[[i]][label_list[[i]],"int"]
            label_df[i,3] <- names(label_list)[[i]]
          }

          names(label_df) <- c("rt", "int", "name")
          label_df[,"name"] <- gsub("SRM SIC ", "", label_df[,"name"])

          if(!is.null(labels)){
            label_df$name <- labels
          }

          plot_multi <- ggplot(data = idn_df, aes_string(x = 'rt', y = 'int', group = 'name', colour = 'name')) +
                            geom_line() + theme_bw() + ggrepel::geom_label_repel(data = label_df, aes_string(x = 'rt', y = 'int', label = 'name')) +
                            guides(group = "none") +
                            guides(colour = "none")  +
                            xlab("Retention Time (mins)") +
                            ylab("Intensity") +
                      theme(axis.text.y = element_text(size = 12, face = "bold"),
                            axis.text.x = element_text(size = 12, face = "bold"),
                            axis.title.y = element_text(size = 12, face = "bold"),
                            axis.title.x = element_text(size = 12, face = "bold"))
          print(plot_multi)
          }
)
