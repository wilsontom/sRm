#' Extract mzML binary arrays
#'
#' Extract all the \code{binaryDataArrays} in the specified \code{mzML} file.
#'
#' @param xmlDoc a \code{xml} document
#' @return a list of two elements; array name and array value. Array name is character vector specifying
#' the type of array (time array or intensity). Array value is a large character vector where each element
#' is the \code{base64} encdoded value of the \code{binaryDataArray}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal
#'
#' @examples \dontrun{
#' library(xml2)
#' xmlDoc <- read_xml("example_file.mzML")
#' xmlPeaks <- binaryArrays(xmlDoc)
#' names(xmlPeaks)
#' [1] "name"  "value"
#'  }

.binaryArrays <- function(xmlDoc)
{
  if(class(xmlDoc)[1] != "xml_document"){
    stop("...xmlDoc must be an xml_documment read in using xml2::read_xml", call. = FALSE)
  }

  arrays <- xml_find_all(xmlDoc, "//d1:binaryDataArray")

  array_type <- lapply(arrays, function(x)(xml_attrs(xml_children(x)[[3]])[["name"]]))

  array_binary_value <- lapply(arrays, function(x)(xml_text(xml_children(x)[[4]])))

  array_res <- list(name = unlist(array_type), value = unlist(array_binary_value))

  return(array_res)
  }
