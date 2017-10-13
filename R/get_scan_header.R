#' Extract scan header information
#'
#' Extract key values (parent m/z, product m/z and polarity) from the \code{chromatogram} blocks of an \code{.mzML} file
#'
#' @param x a valid \code{.mzML} file
#' @return a \code{tibble} containing
#' \itemize{
#'     \item{\code{header}}
#'     \item{\code{polarity}}
#'     \item{\code{Q1}}
#'     \item{\code{Q3}}
#'     \item{\code{tidy_head}}
#' }
#'
#' @export

get_scan_header <- function(x)
  {

  # check that input is mzML file
  is.mzMLfile(x)

  # open mzML file and extract chromatogram headers
  xmltmp <- read_xml(x)

  xml_headers <- xml_find_all(xmltmp, '//d1:chromatogram')

  scan_head <- purrr::map(xml_attrs(xml_headers), ~ {
    .[['id']]
  }) %>% unlist(.)

  all_head_attr <-
    purrr::map(xml_headers, ~ {
      xml_attrs(xml_children(.))
    }) %>% purrr::map(., unlist)

  all_head_attr_tib <-
    purrr::map(all_head_attr, ~ {
      tibble(chrom = .)
    })


  polarity_ont <-
    purrr::map(all_head_attr_tib, ~ {
      filter(., chrom == 'MS:1000129' | chrom == 'MS:1000130')
    })

  polarity_check <- purrr::map(polarity_ont, ~ {
    if (nrow(.) == 0) {
      . <- tibble(chrom = 'tic')
    } else{
      . <- .
    }
  }) %>% bind_rows()

  header_tibble <-
    tibble(header = scan_head, polarity = polarity_check$chrom)

  qmz_tib <- purrr::map(header_tibble$header, ~{get_Qmz(.)}) %>% bind_rows()

  scan_tib <- left_join(header_tibble, qmz_tib, by = 'header') %>% mutate(polarity = replace(polarity, polarity == 'MS:1000129','-')) %>%
    mutate(polarity = replace(polarity, polarity == 'MS:1000130','+'))

  scan_header_clean <- format_scan_header(scan_tib)

  scan_tib2 <- tibble::add_column(scan_tib, tidy_head = scan_header_clean)

  return(scan_tib2)

  }
