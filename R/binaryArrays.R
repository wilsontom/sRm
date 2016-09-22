#' Extract mzML binary arrays
#'
#' Extract all the \code{binaryDataArrays} in the specified \code{mzML} file.
#'
#' @param xmlDoc a \code{xml} document
#' @return a list of three elements; array name, array value and array precision. Array name is character vector specifying
#' the type of array (time array or intensity). Array value is a large character vector where each element
#' is the \code{base64} encdoded value of the \code{binaryDataArray}. Arrray precision is the value specifing the encoded precision
#' of each array (32-bit or 64-bit)
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal
#'
#' @importFrom xml2 xml_find_all xml_attrs xml_children xml_text
#'
#'
#' @examples \dontrun{
#' library(xml2)
#' xmlDoc <- read_xml("example_file.mzML")
#' xmlPeaks <- binaryArrays(xmlDoc)
#' names(xmlPeaks)
#' [1] "name"  "value"
#'  }

binaryArrays <- function(xmlDoc)
{
  if(class(xmlDoc)[1] != "xml_document"){
    stop("...xmlDoc must be an xml_documment read in using xml2::read_xml", call. = FALSE)
  }

  arrays <- xml_find_all(xmlDoc, "//d1:binaryDataArray")

  array_type <- lapply(arrays, function(x)(xml_attrs(xml_children(x)[[3]])[["name"]]))

  array_prec <- lapply(arrays, function(x)(xml_attrs(xml_children(x)[[1]])[["name"]]))

  array_binary_value <- lapply(arrays, function(x)(xml_text(xml_children(x)[[4]])))

  array_res <- list(name = unlist(array_type), value = unlist(array_binary_value), precision = unlist(array_prec))

  ## clean up precisin

  array_res$precision <- gsub("64-bit float", 64, array_res$precision)
  array_res$precision <- gsub("32-bit float", 32, array_res$precision)
  array_res$precision <- as.numeric(array_res$precision)

  return(array_res)
  }
