#' Extract meta information from .mzML file
#'
#' Extract a minimal amount meta data from a `.mzML` file
#'
#' @param x a valid `.mzML` file
#' @return a `tibble` containing;
#'  * mzML Schema
#'  * Acquisition Date
#'  * Acquisition Time
#'  * Instrument Model
#'  * File ID
#'
#' @export

fileMetaData <- function(x)
{
  xmltmp <- xml2::read_xml(x)

  refGroup <-
    xml2::xml_find_all(xmltmp, '//d1:referenceableParamGroup')
  mzml_schema <-
    xml2::xml_attrs(xml2::xml_children(xmltmp)[[1]])[['schemaLocation']]


  InstrumentModel <- detectInstrumentModel(x)
  if(stringr::str_detect(InstrumentModel, 'Shimadzu')) {
    type <- 'lcd'
  } else{
    type <- 'raw'
  }


  if (type == 'raw') {
    InstrumentParam <-
      xml2::xml_find_all(xmltmp, "//d1:referenceableParamGroup")[[1]] %>%
      xml2::xml_children()

    ModelCV <- InstrumentParam[[1]] %>% xml2::xml_attrs() %>%
      .[['accession']]

    SerialCV <- InstrumentParam[[2]] %>% xml2::xml_attrs() %>%
      .[['accession']]

    runHeader <- xml2::xml_find_all(xmltmp, "//d1:run")
    acqStamp <- xml2::xml_attrs(runHeader)[[1]][["startTimeStamp"]]
    acqDate <- strsplit(acqStamp, "T")[[1]][1]
    acqTime <- strsplit(acqStamp, "T")[[1]][2]
    acqTime_sp <- strsplit(acqTime, ":")
    acqTime_hhmm <-
      paste0(acqTime_sp[[1]][1], ":", acqTime_sp[[1]][2])

  }

  if (type == 'lcd') {
    UserParam <-
      xml2::xml_find_all(xmltmp, "//d1:instrumentConfiguration")
    inst_model <-
      xml2::xml_attrs(xml2::xml_children(UserParam)[[1]])[['name']]
  }


  fileInfo <- xml2::xml_find_all(xmltmp, "//d1:sourceFileList")

  fileName <-
    xml2::xml_attrs(xml2::xml_children(fileInfo))[[1]][['name']]
  fileID <-
    xml2::xml_attrs(xml2::xml_children(fileInfo))[[1]][['id']]


  if (type == 'raw') {
    meta_tibble <-
      tibble::tibble(
        name = c(
          'Schema',
          'Datestamp',
          'Timestamp',
          'Instrument',
          'sampleID'
        ),
        value = c(mzml_schema, acqDate, acqTime_hhmm, stringr::str_c(ModelCV,'//', SerialCV), fileName)
      )
  }

  if (type == 'lcd') {
    meta_tibble <-
      tibble::tibble(
        name = c(
          'Schema',
          'Datestamp',
          'Timestamp',
          'Instrument',
          'sampleID'
        ),
        value = c(mzml_schema, NA, NA, inst_model, fileName)
      )
  }


  return(meta_tibble)

}
