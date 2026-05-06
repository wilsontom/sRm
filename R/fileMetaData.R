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

  mzml_schema <-
    xml2::xml_attrs(xml2::xml_children(xmltmp)[[1]])[['schemaLocation']]

  InstrumentModel <- detectInstrumentModel(x)
  runHeader <- xml2::xml_find_first(xmltmp, "//d1:run")
  acqStamp <- xml2::xml_attr(runHeader, "startTimeStamp")

  if (!is.na(acqStamp) && nzchar(acqStamp)) {
    acqDate <- strsplit(acqStamp, "T")[[1]][1]
    acqTime <- strsplit(acqStamp, "T")[[1]][2]
    acqTime_sp <- strsplit(acqTime, ":")
    acqTime_hhmm <- paste0(acqTime_sp[[1]][1], ":", acqTime_sp[[1]][2])
  } else {
    acqDate <- NA_character_
    acqTime_hhmm <- NA_character_
  }

  fileInfo <- xml2::xml_find_all(xmltmp, "//d1:sourceFileList")

  fileName <-
    xml2::xml_attrs(xml2::xml_children(fileInfo))[[1]][['name']]

  if (is.null(fileName) || is.na(fileName) || !nzchar(fileName)) {
    fileName <- basename(x)
  }

  meta_tibble <-
    tibble::tibble(
      name = c(
        'Schema',
        'Datestamp',
        'Timestamp',
        'Instrument',
        'sampleID'
      ),
      value = c(mzml_schema, acqDate, acqTime_hhmm, InstrumentModel, fileName)
    )

  return(meta_tibble)
}
