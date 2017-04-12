#' @rdname combineTransitions
#'
#' @description Each parent \code{m/z} will usually have multiple separate Q3 product ions. SRM transitions are stored separately
#' in the \code{mzML} file. Each index is a transition from a single Q1 precursor ion to one of the specified Q3 ions.
#' This method combines all Q3 product ion chromatograms into one, to produce a total ion chromatogram based on
#' the parent \code{m/z} and all Q3 product ions assigned to that parent \code{m/z}
#'
#' Where a dynamic MRM is used; then the start retention time (Rt) and end Rt is also taken into account when transitions are combined.
#'
#' For example the following transitions from a standard SRM-MS experiment would be combined into a single chromatogram
#' based on the parent m/z value;
#'
#' \code{- SRM SIC 153.01,65.271} \cr
#' \code{- SRM SIC 153.01,67.232} \cr
#' \code{- SRM SIC 153.01,109.094} \cr
#'
#' Whereas the following transitions from a dynamic SRM-MS method would be combined into two different SRM TICs based on their
#' parent m/z value and retention time window;
#'
#' \code{- SRM SIC Q1=145 Q3=56.996 start=10.61683333 end=20.84128333} \cr
#' \code{- SRM SIC Q1=145 Q3=100.996 start=10.61665 end=20.84081667} \cr
#' \code{- SRM SIC Q1=145 Q3=108.996 start=4.5304 end=10.53233333} \cr
#' \code{- SRM SIC Q1=145 Q3=127.096 start=4.530016667 end=10.53213333} \cr
#'
#' The above transitions would be combined to make the following;
#'
#' \code{Q1: 145 -> Q3: 56.996//100.996 (10.6 - 20.8)} \cr
#' \code{Q1: 145 -> Q3: 108.996//127.096 (4.5 - 10.5)} \cr

setMethod(f = combineTransitions, signature = "SRM",
          function(object){

          if(length(grep("Q1",object@filter))!= 0){
            unique_scan_filter <- unique_idrefs(object)
          }else{
            unique_scan_filter <- object@header$parent
          }

          trans_sets <- split(object@peaks, unique_scan_filter)

          trans_header <- split(object@header, unique_scan_filter)

          trans_dim <- NULL
          for(i in seq_along(trans_sets)){
            trans_dim[[i]] <- lapply(trans_sets[[i]], nrow)
          }

          equality_test <- lapply(trans_dim, function(x)(length(unique(x))))
          idx <- which(equality_test != 1)

          if(length(idx != 0)){
            message("WARNING: The follwing transitions have been removed from the 'transition' object:")
            cat("\n")
            for (i in seq_along(idx)){
              idx_n <- as.numeric(rownames(trans_header[idx][[i]]))
                for(k in seq_along(idx_n)){
                  message(object@filter[idx_n[k]], "\n")
                }
              cat("\n")
            }
          }

          if(length(idx != 0)){
            trans_sets <- trans_sets[-idx]
            trans_header <- trans_header[-idx]
          }

          trans_dfs <- lapply(trans_sets, function(x)(do.call("cbind",x)))

          int_idx <- rt_idx <- NULL
          for(i in seq_along(trans_dfs)){
            int_idx[[i]] <- grep("int", names(trans_dfs[[i]]))
            rt_idx[[i]] <- grep("rt", names(trans_dfs[[i]]))
          }

          int_sum <- NULL
          for(i in seq_along(trans_dfs)){
            if(length(int_idx[[i]]) == 1){
              int_sum[[i]] <- trans_dfs[[i]][,int_idx[[i]]]
            }else{
              int_sum[[i]] <- apply(trans_dfs[[i]][,int_idx[[i]]],1,sum)
            }
          }

          trans_df_comb <- list()
          for(i in seq_along(int_sum)){
            trans_df_comb[[i]] <- data.frame(rt = trans_dfs[[i]][,rt_idx[[i]]][[1]],int = trans_dfs[[i]][,int_idx[[i]]], TIC = int_sum[[i]])

            nmidx <- which(names(trans_df_comb[[i]]) != "rt" | names(trans_df_comb[[i]]) != "TIC")

            names(trans_df_comb[[i]])[-nmidx] <- "int"


            #names(trans_df_comb[[i]]) <- gsub(".int", "", names(trans_df_comb[[i]]))
            #patsub <- paste0(".",unique(trans_header[[i]]$parent))
            #names(trans_df_comb[[i]]) <- gsub(patsub,"", names(trans_df_comb[[i]]))
            #names(trans_df_comb[[i]]) <- gsub("int.", "int-", names(trans_df_comb[[i]]))
          }

          new_names <- NULL
          for(i in seq_along(trans_header)){
            pMz <- paste("Q1", trans_header[[i]][1,"parent"], sep = ": ")
            qMzs <- paste(trans_header[[i]][,"product"], collapse = "//")
            qMz2 <- paste("Q3", qMzs, sep = ": ")
            new_names[[i]] <- paste(pMz, qMz2, sep = " -> ")
          }

          names(trans_df_comb) <- new_names
          class(trans_df_comb) <- "transition"

          return(trans_df_comb)
          }
)
