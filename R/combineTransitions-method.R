#' Combine Transitions
#' @rdname combineTransitions
#'
#' @description While each parent \code{m/z} will usually have three separat Q3 product ions, SRM transitions are store separately
#' in the \code{mzML} file. Each index is a single transition of parent \code{m/z} to one of the specified Q3 \code{m/z}.
#' This method simply combins all Q3 product ion chromatograms into one, to produce a total ion chromatogram based on
#' the parent \code{m/z} and all Q3 product ions assigned to that parent \code{m/z}
#'
setMethod(f = combineTransitions, signature = "SRM",
          function(object){

          trans_sets <- split(object@peaks, object@header$parentMz)

          trans_header <- split(object@header, object@header$parentMz)

          trans_dfs <- lapply(trans_sets, function(x)(do.call("cbind",x)))

          int_idx <- grep("int", names(trans_dfs[[1]]))
          int_rt <- grep("rt", names(trans_dfs[[1]]))

          int_sum <- lapply(trans_dfs,function(x)(apply(x[,int_idx],1,sum)))

          trans_df_comb <- list()
          for(i in 1:length(int_sum)){
            trans_df_comb[[i]] <- data.frame(rt = trans_dfs[[i]][,int_rt[1]], TIC = int_sum[[i]])
          }

          new_names <- NULL
          for(i in 1:length(trans_header)){
            pMz <- paste("Q1", trans_header[[i]][1,"parentMz"], sep = ": ")
            qMzs <- paste(trans_header[[i]][1,"Q3mz"],
                            trans_header[[i]][2,"Q3mz"],
                              trans_header[[i]][3,"Q3mz"], sep = "/")
            qMz2 <- paste("Q3", qMzs, sep = ": ")
            new_names[[i]] <- paste(pMz, qMz2, sep = " -> ")
          }

          names(trans_df_comb) <- new_names

          return(trans_df_comb)
          }
)
