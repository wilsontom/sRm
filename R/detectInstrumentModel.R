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

  instrument_model <-
    xml2::xml_find_first(
      xml_tmp,
      "//d1:referenceableParamGroup/d1:userParam[@name='instrument model']"
    ) %>%
    xml2::xml_attr("value")

  if (!is.na(instrument_model) && nzchar(instrument_model)) {
    return(instrument_model)
  }

  instrument_model <-
    xml2::xml_find_first(
      xml_tmp,
      "//d1:instrumentConfiguration/d1:cvParam[contains(@name, 'instrument model')]"
    ) %>%
    xml2::xml_attr("name")

  if (!is.na(instrument_model) &&
      nzchar(instrument_model) &&
      instrument_model != "instrument model") {
    return(instrument_model)
  }

  instrument_model <-
    xml2::xml_find_first(
      xml_tmp,
      "//d1:referenceableParamGroup/d1:cvParam[contains(@name, 'instrument model')]"
    ) %>%
    xml2::xml_attr("name")

  if (!is.na(instrument_model) && nzchar(instrument_model)) {
    return(instrument_model)
  }

  return("no info found")
}
