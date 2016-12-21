#' Open a SRM-MS file
#'
#' Open and parse \code{mzML} file into \code{SRM} object
#'
#' @param filename a \code{mzML} file
#' @return a \code{SRM} object
#'  (see \code{\link{SRM-class}})
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @importFrom methods new
#' @importFrom xml2 read_xml

openSRMfile <- function(filename)
{

  xml_tmp <- read_xml(filename)
  cv_params <- cvParams(xml_tmp)
  id_refs <- idRefs(xml_tmp)
  binary_arrays <- binaryArrays(xml_tmp)

  if(cv_params$name[13] == "no compression"){compression = "none"}

  bin_vals <- binary_arrays$value
  bin_prec <- binary_arrays$precision

  bin_df <- data.frame(name = binary_arrays$name, prec = bin_prec)
  bin_df <- unique(bin_df)

  bin_df$prec <- paste(bin_df$prec, "bit float", sep = " ")

  peaks <- list()
  for(i in 1:length(bin_vals)){
    peaks[[i]] <- decodePeaks(bin_vals[[i]], compression = compression, size = bin_prec[[i]] / 8)
  }

  names(peaks) <- binary_arrays$name

  peaks_time <- peaks[which(names(peaks) == "time array")]
  peaks_int <- peaks[which(names(peaks) == "intensity array")]

  peaks_df <- list()
  for(i in 1:length(peaks_time)){
    peaks_df[[i]] <- data.frame(rt = peaks_time[[i]], int = peaks_int[[i]])
  }

  cv_idx <- as.character(unique(cv_params$accession))

  cv_unique <- cv_params[match(cv_idx, cv_params$accession),]

  cv_unique <- cv_unique[-which(cv_unique$accession == "MS:1000827"),]


  meta_data <- list()
  #precision <- cv_unique[grep("float", cv_unique$name),]
  precision <- NULL
  for(i in 1:nrow(bin_df)){
    precision[[i]] <- paste(bin_df[i,"name"], bin_df[i, "prec"], sep = " : ")
  }

  inst_model <- cv_unique[grep("serial number", cv_unique$name),]
  compression <- cv_unique[grep("compression", cv_unique$name),]
  schema <- xml_attrs(xml_children(xml_tmp)[[1]])[["schemaLocation"]]
  file_id <- xml_attrs(xml_children(xml_tmp)[[1]])[["id"]]

  meta_data$fileID <- file_id
  meta_data$precision <- precision
  meta_data$compressin <- as.character(compression$name)
  meta_data$schema <- schema
  meta_data$instrument_model <- as.character(inst_model$value)


  object <- new("SRM")

  object@peaks <- peaks_df[-1]
  object@totalIonCount <- peaks_df[[1]]
  object@index <- id_refs[-1]
  object@SHA1 <- as.character(cv_params[which(cv_params$name == "SHA-1"),"value"])
  object@meta <- meta_data

  names(object@peaks) <- object@index

  header <- data.frame(scanIndex = object@index, parentMz = "", Q3mz = "", totalIonCount = "", basePeakInt = "")

  index_clean <- gsub("SRM SIC","", header$scanIndex)
  index_clean <- gsub("[[:space:]]", "", index_clean)
  index_split <- strsplit(index_clean, ",")

  pMz <- Qmz <- NULL
  for(i in 1:length(index_split)){
    pMz[i] <- index_split[[i]][[1]]
    Qmz[i] <- index_split[[i]][[2]]
  }

  tic <- as.vector(sapply(object@peaks, function(x)(sum(x[,2]))))
  bpi <- as.vector(sapply(object@peaks, function(x)(max(x[,2]))))

  header$parentMz <- pMz
  header$Q3mz <- Qmz
  header$totalIonCount <- tic
  header$basePeakInt <- bpi
  object@header <- header

  return(object)
}
