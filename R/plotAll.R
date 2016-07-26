#' @rdname plotAll
#' @export

plotAll.transition <- function(x, n)
{

  envir = .GlobalEnv
  plotInd <- function(x,id)
  {
    plot_a <- ggplot(data = x, aes(x = rt, y = int)) + geom_line(size = 0.5) + theme_bw() +
      theme(legend.position = "none") +
      theme(strip.text.x = element_text(size=12)) +
      theme(axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.title.y = element_blank(),
            axis.title.x = element_blank()) +
      ggtitle(id)
    return(plot_a)
  }


  trans_n <- x[[n]]

  tic_df <- data.frame(rt = trans_n[,"rt"], int = trans_n[,"TIC"])

  idn <- which(names(trans_n) != "rt" & names(trans_n) != "TIC")

  ind_df <- trans_n[,idn]

  ind_list <- list()
  for(i in 1:length(ind_df)){
    ind_list[[i]] <- data.frame(rt = trans_n[,"rt"], int = ind_df[,i])
  }

  plot_tic <- plotInd(tic_df, "")

  ind_names_a <- strsplit(names(x)[n], " Q3:")[[1]][2]
  ind_names_b <- sub("^\\s+", "", ind_names_a)
  ind_names_c <- strsplit(ind_names_b, "//")[[1]]


  plot_inds <- list()
  for(i in 1:length(ind_list)){
    plot_inds[[i]] <- plotInd(ind_list[[i]], id = ind_names_c[i])
  }


  names(plot_inds) <- seq(from = 1, to = length(plot_inds), by = 1)

  ind_ob <- lapply(seq_along(plot_inds),
                   function(i,x){
                     assign(paste0("srm_plot",i), x[[i]], envir = .GlobalEnv)},
                   x = plot_inds)

  ind_env <- ls(pattern = "srm_plot", envir = .GlobalEnv)

  main_title <- names(x)[n]

  ind_ncol = as.numeric(length(ind_env))

  grob_ind <- arrangeGrob(grobs = lapply(ind_env, function(x){eval(parse(text = x))}), nrow = 1, ncol = ind_ncol)

  plot_layer_1 <- grid.arrange(heights = unit(c(1,6), "null"), grid.text(main_title, hjust = 0.4, vjust = 0.5), plot_tic)

  plot_layer_2 <- grid.arrange(heights = unit(c(6,6), "null"), plot_layer_1, grob_ind)

  plot_layer_3 <- grid.arrange(widths = unit(c(0.3,6), "null"), ncol = 2, grid.text("Intensity", rot = 90), plot_layer_2)

  plot_layer_4 <- grid.arrange(heights = unit(c(6,0.5), "null"), nrow = 2, plot_layer_3, grid.text("Retention Time (mins)", hjust = 0.2, vjust = 0))

  return(invisible(NULL))

}
