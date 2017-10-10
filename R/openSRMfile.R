#' Open a SRM-MS file
#'
#' Open and parse \code{mzML} file into a \code{SRM} object
#'
#' @param mzMLFile a valid \code{.mzML} file
#' @return a \code{SRM} object
#'  (see \code{\link{SRM-class}})
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @importFrom methods new
#' @importFrom xml2 read_xml
#' @importFrom dplyr tibble filter bind_rows %>% mutate replace


openSRMfile <- function(mzMLFile)
{
  opentmp <- mzR::openMSfile(mzMLFile, backend = "pwiz")
  chromtmp <- mzR::chromatogram(opentmp)
  chromtib <-
    purrr:::map(chromtmp, ~ {
      tibble(rt = .[, 1], int = .[, 2])
    })

  if (names(chromtmp[[1]])[2] == 'TIC') {
    chromtib[[1]] <- NULL
  }

  object <- new("SRM")
  object@peaks <- chromtib
  object@SHA1 <- get_sha1(mzMLFile)

  object@totIonCount <- chromtmp[[1]]
  names(object@totIonCount) <- c('rt', 'int')

  scan_head_tmp <- get_scan_header(x)

  if (scan_head_tmp$header[1] == 'TIC') {
    scan_head_tmp <- scan_head_tmp[-1, ]
  }
  object@filter <- scan_head_tmp$header
  object@index <- scan_head_tmp$tidy_head

  header_tmp <-
    purrr::map(object@peaks, ~ {
      tibble(totIonCount = sum(.$int),
             basePeakInt = max(.$int))
    }) %>% bind_rows() %>% tibble::add_column(., header = scan_head_tmp$header)

  scan_head_tmp <- scan_head_tmp  %>% left_join(header_tmp)

  scan_head_tmp$tidy_head <- NULL
  ob_header <-
    scan_head_tmp %>% mutate(polarity = replace(polarity, polarity == '+', '1')) %>% mutate(polarity = replace(polarity, polarity == '-', '-1'))
  names(scan_head_tmp)[c(1,3:4)] <- c('filter', 'parent', 'product')

  object@header <- scan_head_tmp

  object_meta <- get_meta(mzMLFile)

  object@meta <- as.list(object_meta$value)
  names(object@meta) <- object_meta$name

  return(object)
}

