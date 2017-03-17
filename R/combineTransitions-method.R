#' @rdname combineTransitions
#'
#' @description Each parent \code{m/z} will usually have multiple separate Q3 product ions. SRM transitions are stored separately
#' in the \code{mzML} file. Each index is a transition from a single Q1 precursor ion to one of the specified Q3 ions.
#' This method combines all Q3 product ion chromatograms into one, to produce a total ion chromatogram based on
#' the parent \code{m/z} and all Q3 product ions assigned to that parent \code{m/z}
#'
setMethod(f = combineTransitions, signature = "SRM",
          function(object){

          trans_sets <- split(object@peaks, object@header$parentMz)

          trans_header <- split(object@header, object@header$parentMz)

          trans_dfs <- lapply(trans_sets, function(x)(do.call("cbind",x)))

          int_idx <- rt_idx <- NULL
          for(i in 1:length(trans_dfs)){
            int_idx[[i]] <- grep("int", names(trans_dfs[[i]]))
            rt_idx[[i]] <- grep("rt", names(trans_dfs[[i]]))
          }

          int_sum <- NULL
          for(i in 1:length(trans_dfs)){
            int_sum[[i]] <- apply(trans_dfs[[i]][,int_idx[[i]]],1,sum)
          }

          trans_df_comb <- list()
          for(i in 1:length(int_sum)){
            trans_df_comb[[i]] <- data.frame(rt = trans_dfs[[i]][,rt_idx[[i]]][[1]],int = trans_dfs[[i]][,int_idx[[i]]], TIC = int_sum[[i]])
            names(trans_df_comb[[i]]) <- gsub(".int", "", names(trans_df_comb[[i]]))
            patsub <- paste0(".",unique(trans_header[[i]]$parentMz))
            names(trans_df_comb[[i]]) <- gsub(patsub,"", names(trans_df_comb[[i]]))
            names(trans_df_comb[[i]]) <- gsub("int.", "int-", names(trans_df_comb[[i]]))
          }

          new_names <- NULL
          for(i in 1:length(trans_header)){
            pMz <- paste("Q1", trans_header[[i]][1,"parentMz"], sep = ": ")
            qMzs <- paste(trans_header[[i]][,"Q3mz"], collapse = "//")
            qMz2 <- paste("Q3", qMzs, sep = ": ")
            new_names[[i]] <- paste(pMz, qMz2, sep = " -> ")
          }

          names(trans_df_comb) <- new_names
          class(trans_df_comb) <- "transition"

          return(trans_df_comb)
          }
)
