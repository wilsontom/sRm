#' Decocde peaks
#'
#' Decode a \code{binaryDataArray} into a vector of either time or intensity
#'
#' @param xmlDoc a \code{base64} encoded vector
#' @param compression compresison type; \code{default = "none"}
#' @param size a numeric value for the number of bytes per element in the byte stream
#' @return a numeric vector
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal
#'
#' @examples \dontrun{
#' library(xml2)
#' xmlDoc <- read_xml("example_file.mzML")
#' xmlPeaks <- binaryArrays(xmlDoc)
#' intensity <- decodePeaks(xmlPeaks$value[4],size = 8)
#' head(intensity)
#' [1] 3.516104 3.427163 3.493258 3.397190 3.430998 3.053472
#' }

decodePeaks <- function(x, compression = "none", size)
{
  x <- base64enc::base64decode(x)
  raw_x <- as.raw(x)

  raw_x2 <- memDecompress(from = raw_x, type = compression)

  bins_x <- readBin(raw_x2, what = "double", n = length(raw_x2)/size, size = size, endian = .Platform$endian)

  return(bins_x)
}
