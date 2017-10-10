#' Check file is .mzML
#'
#' @param x a valid \code{.mzML} file
#' @return NULL
#'
#' @keywords internal


is.mzMLfile <- function(x)
{
  strl <- length(strsplit(basename(x), '\\.')[[1]])

  if (strsplit(basename(x), '\\.')[[1]][strl] != 'mzML') {
    stop(deparse(substitute(x)), ' is not a .mzML file')
  }
  return(invisible(NULL))
}



#' Format scan header
#'
#' @keywords internal

format_scan_header <- function(x)
{
  if (!isTRUE(tibble::is.tibble(x))) {
    stop(deparse(substitute(x)), ' should be a `tibble`', call. = FALSE)
  }

  tidy_header <- NULL
  for (i in seq_along(x$header)) {
    if (x[i, 'header'] == 'TIC') {
      tidy_header[[i]] <- 'TIC'
    } else{
      tidy_header[[i]] <-
        paste0('Q1: ', x[i, 'Q1'], ' --> Q3:', x[i, 'Q3'], ' (', x[i, 'polarity'], ')')
    }

  }

  return(tidy_header)
}


#' Extract all mzML cvParams
#' @keywords internal

get_all_cvParams <- function(x)
    {

  xml_headers  <- xml_find_all(x, '//d1:cvParam') %>% purrr::map(., xml_attrs)

  cv_params <- purrr:::map(xml_headers,  ~{tibble(accession = .[['accession']], value = .[['value']])}) %>% bind_rows()

  return(cv_params)

  }







