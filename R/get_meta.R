#' Extract meta information from .mzML file
#'
#' Extract a minimal amount meta data from a `.mzML` file
#'
#' @param x a valid `.mzML` file
#' @return a `tibble` containing;
#' \itemize{
#'     \item{`mzML Schema`}
#'     \item{`Acquisition Date`}
#'     \item{`Acquisition Time`}
#'     \item{`Instrument Model`}
#'     \item{`File ID`}
#' }
#'
#' @export

get_meta <- function(x)
{
  xmltmp <- xml2::read_xml(x)

  refGroup <-
    xml2::xml_find_all(xmltmp, '//d1:referenceableParamGroup')
  mzml_schema <-
    xml2::xml_attrs(xml2::xml_children(xmltmp)[[1]])[['schemaLocation']]

  UserParam <- xml2::xml_find_all(xmltmp, "//d1:userParam")
  inst_model <- xml2::xml_attrs(UserParam)[[1]][['value']]

  runHeader <- xml2::xml_find_all(xmltmp, "//d1:run")
  acqStamp <- xml2::xml_attrs(runHeader)[[1]][["startTimeStamp"]]
  acqDate <- strsplit(acqStamp, "T")[[1]][1]

  acqTime <- strsplit(acqStamp, "T")[[1]][2]
  acqTime_sp <- strsplit(acqTime, ":")
  acqTime_hhmm <-
    paste0(acqTime_sp[[1]][1], ":", acqTime_sp[[1]][2])

  fileInfo <- xml2::xml_find_all(xmltmp, "//d1:sourceFileList")

  fileName <- xml2::xml_attrs(xml_children(fileInfo))[[1]][['name']]
  fileID <- xml2::xml_attrs(xml_children(fileInfo))[[1]][['id']]


  meta_tibble <-
    tibble::tibble(
      name = c('Schema', 'Datestamp', 'Timestamp', 'Instrument', 'sampleID'),
      value = c(mzml_schema, acqDate, acqTime_hhmm, inst_model, fileName)
    )

  return(meta_tibble)

}
