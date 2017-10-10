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


#' Parse Q1 and Q3 mass values from scan filter string
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

  scan_index_tib <- tibble(header = x, Q1 = Q1value, Q3 = Q3value)

  return(scan_index_tib)


}


#' SHA-1
#'
#'
#'
#'




get_sha1 <- function(x)
{

  xmltmp <- read_xml(x)


  cv_params <- get_all_cvParams(xmltmp)

  SHA1 <- filter(cv_params, accession == 'MS:1000569')$value

  return(SHA1)


}





