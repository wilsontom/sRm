#' Detect Instrument Model
#'
#'
#' @param mzml_file the absolute file path of a valid `.mzML` file
#' @return a character string of the MS cvParam description for the instrument model
#'
#' @export

detectInstrumentModel <- function(mzml_file)
{
  xml_tmp <- xml2::read_xml(mzml_file)

  instrument_cv <-
    as.character(
      xml_tmp %>%
        xml2::xml_find_all(., "//d1:instrumentConfiguration") %>%
        xml2::xml_contents() %>%
        xml2::xml_attr('accession')
    )


  if (length(instrument_cv) > 0) {
    instrument_cv <- instrument_cv[which(instrument_cv != 'NA')]
  }

  if(all(is.na(instrument_cv))) {
    return('no info found')
  }


  api_url <-
    stringr::str_c('http://www.ebi.ac.uk/ols/api/terms?id=', instrument_cv)

  api_query <- httr::GET(api_url) %>% httr::content(., 'parsed')

  cv_description <-
    api_query$`_embedded`$terms[[1]]$description[[1]]

  return(cv_description)


}
