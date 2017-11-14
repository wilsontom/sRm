#' Set global variables
#' @keywords internal
globalVariables(
  c(
    '.',
    'accession',
    'polarity',
    'PrecursorCharge',
    'PrecursorMz',
    'PrecursorName',
    'PrecursorRT',
    'ProductCharge',
    'ProductMz',
    'name',
    'parent',
    'product',
    'rt',
    'int',
    'totIonCount',
    'basePeakInt'
  )
)


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
#' This function is only used internally in \code{get_scan_header}
#' @param x a \code{tibble}
#' @return a vector of tidy scan filters
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


#' Extract all .mzML cvParams
#'
#' Extract the accession code and value for all \code{cvParams} in the \code{.mzML} file
#'
#' @param x a valid \code{.mzML} file
#' @return a \code{tibble} of \code{cvParams} accessions and values
#'
#' @keywords internal

get_all_cvParams <- function(x)
{
  xml_headers  <-
    xml_find_all(x, '//d1:cvParam') %>% purrr::map(., xml_attrs)

  cv_params_acc <-
    purrr::map(xml_headers,  ~ {
      .[['accession']]
    }) %>% unlist(.) %>% as_tibble(.)

  cv_params <-
    purrr::map(xml_headers,  ~ {
      .[['value']]
    }) %>% unlist(.) %>% as_tibble(.) %>% tibble::add_column(., accession = cv_params_acc$value, .before = 'value')

 #cv_params <- as_tibble(accession = cv_params_acc, value = cv_params_value) # stringsAsFactors = FALSE)


  return(cv_params)

}

#' Get Parent (Q1) and Product (Q3) mass values
#'
#' Parse Q1 and Q3 mass values from scan filter string. This function is only used internally in \code{get_scan_header}
#'
#' @param x a vector of scan filters
#' @return a \code{data.frame} of Q1 and Q3
#' @keywords internal

get_Qmz <- function(x)
{
  xsplit <- strsplit(x, " ")[[1]]

  xsplit_mz <- xsplit[length(xsplit)]

  qsplit <- strsplit(xsplit_mz, ",")

  if (length(qsplit[[1]]) != 2 & qsplit[[1]][[1]] != 'TIC') {
    message('WARNING: length of `qsplit` should be 2 (Q1 and Q3 m/z); potential parsing errror')
  }

  if (qsplit[[1]][[1]] != 'TIC') {
    Q1value <- qsplit[[1]][1]
    Q3value <- qsplit[[1]][2]
  }

  if (qsplit[[1]][[1]] == 'TIC') {
    Q1value <- NA
    Q3value <- NA

  }

  scan_index_df <- data.frame(header = x, Q1 = Q1value, Q3 = Q3value, stringsAsFactors = FALSE)


  return(scan_index_df)
}


#' Extract the SHA-1 value
#'
#' Extract the SHA-1 hash value for a \code{.mzML} file
#'
#' @param x \ valid \code{.mzML} file
#' @return a character string of the SHA-1
#' @keywords internal

get_sha1 <- function(x)
{
  xmltmp <- read_xml(x)

  cv_params <- get_all_cvParams(xmltmp)

  SHA1 <- filter(cv_params, accession == 'MS:1000569')$value

  return(SHA1)
}



