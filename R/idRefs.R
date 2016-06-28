#' Extract mzML idRefs
#'
#' Extract all the \code{idRefs} in the specified \code{mzML} file. The \code{idRefs} will correspond to the individual scan events. There will be a single \code{idRef} for each
#' SRM transition. The first \code{idRef} is assigned to the total ion count (TIC) chromatogram of the file.
#'
#' @param xmlDoc a \code{xml} document
#' @return a character vector of all available \code{idRefs}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#'
#' @examples \dontrun{
#' library(xml2)
#' xmlDoc <- read_xml("example_file.mzML")
#' xmlRefs <- idRefs(xmlDoc)
#' head(xmlRefs)
#' [1] "TIC"                     "SRM SIC 153.01,65.271"
#' [3] "SRM SIC 153.01,67.232"   "SRM SIC 153.01,109.094"
#' [5] "SRM SIC 179.022,107.007" "SRM SIC 179.022,134.006"
#'  }

idRefs <- function(xmlDoc)
  {
  if(class(xmlDoc)[1] != "xml_document"){
    stop("...xmlDoc must be an xml_documment read in using xml2::read_xml", call. = FALSE)
  }

  refs <- xml_find_all(xmlDoc, "//d1:offset")
  ref_names <- sapply(refs, function(x)(xml_attrs(x)[["idRef"]]))
  return(ref_names)
  }
