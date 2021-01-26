#' show-SRM
#' @rdname show
#'
#' @param object a SRM object
#' @return NULL
#' @export
#' @importFrom methods show new
#' @importFrom utils object.size

setMethod("show", signature = "SRM",
          function(object) {
            cat(cli::rule(
              left = crayon::bold('SRM Object'),
              right = paste0('sRm v', utils::packageVersion('sRm'))
            ), '\n', '\n')

            cat(crayon::yellow('SRM File Inputs:', nrow(object@meta), '\n', '\n'))

            trcnt <-
              object@transitions %>% dplyr::filter(index != 'TIC') %>% nrow()

            cat(crayon::yellow('Transitions measured:', trcnt, '\n', '\n'))

            cat(crayon::red(
              'Object Size:',
              format(utils::object.size(object), units = 'Mb'),
              '\n'
            ))
          })
