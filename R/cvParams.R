#' Extract mzML cvParams
#'
#' Extract all the \code{cvParams} in the specified \code{mzML} file.
#'
#' @param xmlDoc a \code{xml} document
#' @return a \code{data.frame} of all avaialble \code{cvParams}. Accession, name and value are all given for each \code{cvParam}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal
#'
#' @examples \dontrun{
#' library(xml2)
#' xmlDoc <- read_xml("example_file.mzML")
#' xmlParams <- cvParams(xmlDoc)
#' head(xmlParms)
#' accession                                      name                                    value
#' 1 MS:1001473 selected reaction monitoring chromatogram
#' 2 MS:1000768                    Thermo nativeID format
#' 3 MS:1000563                         Thermo RAW format
#' 4 MS:1000569                                     SHA-1 42823de9154df2488f6bb5f1e5bdbbb84195718d
#' 5 MS:1000492          Thermo Electron instrument model
#' 6 MS:1000529                  instrument serial number                                 TQU01681
#' }

.cvParams <- function(xmlDoc)
{
  if(class(xmlDoc)[1] != "xml_document"){
    stop("...xmlDoc must be an xml_documment read in using xml2::read_xml", call. = FALSE)
  }

  params <- xml_find_all(xmlDoc, "//d1:cvParam")
  params_acc <- sapply(params, function(x)(xml_attrs(x)[["accession"]]))
  params_name <- sapply(params, function(x)(xml_attrs(x)[["name"]]))
  params_values <- sapply(params, function(x)(xml_attrs(x)[["value"]]))

  cvDF <- data.frame(accession = params_acc, name = params_name, value = params_values)

  return(cvDF)
}

