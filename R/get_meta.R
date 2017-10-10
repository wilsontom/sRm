#' Extract meta information from .mzML file
#'





get_meta <- function(x)
  {

  is.mzMLfile(x)

  xmltmp <- read_xml(x)

  refGroup <- xml_find_all(xmltmp, '//d1:referenceableParamGroup')
  mzml_schema <- xml_attrs(xml_children(xmltmp)[[1]])[['schemaLocation']]

  UserParam <- xml_find_all(xmltmp, "//d1:userParam")
  inst_model <- xml_attrs(UserParam)[[1]][['value']]

  runHeader <- xml_find_all(xmltmp, "//d1:run")
  acqStamp <- xml_attrs(runHeader)[[1]][["startTimeStamp"]]
  acqDate <- strsplit(acqStamp, "T")[[1]][1]

  acqTime <- strsplit(acqStamp, "T")[[1]][2]
  acqTime_sp <- strsplit(acqTime, ":")
  acqTime_hhmm <- paste0(acqTime_sp[[1]][1], ":", acqTime_sp[[1]][2])

  fileInfo <- xml_find_all(xmltmp, "//d1:sourceFileList")

  fileName <- xml_attrs(xml_children(fileInfo))[[1]][['name']]
  fileID <- xml_attrs(xml_children(fileInfo))[[1]][['id']]


  meta_tibble <- tibble(name = c('mzml_schema', 'acqusition_date', 'acqusition_time', 'instrument_model', 'file_id'), value = c(mzml_schema, acqDate, acqTime_hhmm, inst_model, fileID))

  return(meta_tibble)


  }