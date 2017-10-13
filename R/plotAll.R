#' @rdname plotAll
#' @export
#' @importFrom ggplot2 ggplot aes_string theme_bw geom_line element_text element_blank ggtitle theme xlab ylab
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom grid grid.text unit

plotAll.transition <- function(x, n)
{
  envir = .GlobalEnv
  plotInd <- function(x, id)
  {
    plot_a <-
      ggplot(data = x, aes_string(x = 'rt', y = 'int')) + geom_line(size = 0.4) + theme_bw() +
      theme(legend.position = "none") +
      theme(strip.text.x = element_text(size = 11)) +
      theme(
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      ) +
      ggtitle(id)
    return(plot_a)
  }


  trans_n <- x[[n]]

  tic_df <- data.frame(rt = trans_n[, "rt"], int = trans_n[, "tic"])

  idn <- which(names(trans_n) != "rt" & names(trans_n) != "tic")

  ind_df <- trans_n[, idn]

  ind_list <- list()
  for (i in 1:length(ind_df)) {
    ind_list[[i]] <- data.frame(rt = trans_n[, "rt"], int = ind_df[, i])
  }

  plot_tic <- plotInd(tic_df, names(x)[n])

  ind_names_a <- strsplit(names(x)[n], " Q3:")[[1]][2]

  # remove the polarity
  ind_names_b <- strsplit(ind_names_a, '\\(')[[1]][1]


  #ind_names_b <- sub("^\\s+", "", ind_names_a)
  ind_names_c <- strsplit(ind_names_b, "//")[[1]]


  plot_inds <- list()
  for (i in 1:length(ind_list)) {
    plot_inds[[i]] <- plotInd(ind_list[[i]], id = ind_names_c[i])
  }


  names(plot_inds) <- seq(from = 1,
                          to = length(plot_inds),
                          by = 1)

  ind_ob <- lapply(seq_along(plot_inds),
                   function(i, x) {
                     assign(paste0("srm_plot", i), x[[i]], envir = .GlobalEnv)
                   },
                   x = plot_inds)

  ind_env <- ls(pattern = "srm_plot", envir = .GlobalEnv)

  main_title <- names(x)[n]

  ind_ncol = as.numeric(length(ind_env))

  grob_ind <-
    arrangeGrob(
      grobs = lapply(ind_env, function(x) {
        eval(parse(text = x))
      }),
      nrow = 1,
      ncol = ind_ncol
    )


  plot_layer_1 <-
    arrangeGrob(heights = unit(c(6, 6), "null"), plot_tic)
  plot_layer_2 <-
    arrangeGrob(heights = unit(c(6, 6), "null"), plot_tic, grob_ind)

  grid.arrange(arrangeGrob(
    plot_layer_2,
    left = textGrob("Intensity", rot = 90, vjust =
                      1),
    bottom = textGrob('Retention Time (mins)', hjust = 0.5)
  ))

  return(invisible(NULL))

}
