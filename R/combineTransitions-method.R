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

setMethod(f = combineTransitions, signature = "SRM",
          function(object) {
            parent_index <-
              tibble(parent = unique(object@header$parent),
                     index = seq(from = 1, to = length(unique(
                       object@header$parent
                     ))))

            idn <- seq(from = 1, to = nrow(object@header))

            index_value <-
              parent_index$index[match(object@header$parent, parent_index$parent)]

            tmp_header <-
              data.frame(object@header, spind = index_value, idn = idn)

            split_header <- split(tmp_header, tmp_header$spind)


            peak_groups <- NULL
            for (i in seq_along(split_header)) {
              peak_groups[[i]] <- object@peaks[split_header[[i]][['idn']]]

            }

            combn_trans <- function(x, y)  {
              list_idn <- length(x)
              df_tmp <-
                data.frame(matrix(nrow = nrow(x[[1]]), ncol = (list_idn + 1)))
              names(df_tmp)[1] <- 'rt'

              for (i in seq_along(x)) {
                df_tmp[, 1] <- x[[1]]['rt']
                df_tmp[, i + 1] <- x[[i]]['int']

              }

              product_names <- NULL
              for (i in seq_along(y$product)) {
                product_names[i] <- paste0('int-', y$product[i])
              }

              names(df_tmp)[2:ncol(df_tmp)] <- product_names

              return(df_tmp)

            }

            combined_transitions <- NULL
            for (i in seq_along(peak_groups)) {
              combined_transitions[[i]] <-
                combn_trans(peak_groups[[i]], split_header[[i]])
            }

            for (i in seq_along(combined_transitions)) {
              rtid <- which(names(combined_transitions[[i]]) == 'rt')
              tmp <- combined_transitions[[i]][, -rtid]
              tic <- apply(tmp, 1, sum)
              combined_transitions[[i]] <-
                data.frame(combined_transitions[[i]], tic = tic)
            }
            return(combined_transitions)

          })
