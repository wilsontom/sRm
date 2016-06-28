#' Open a SRM-MS file
#'
#' Function for reading and parsing SRM-MS data in a \code{mzML} file.
#'
#' @param filename a \code{mzML} file
#' @return a \code{SRM} object
#'  (see \code{\link{SRM-class}})
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

openSRMfile <- function(filename)
  {

  xml_tmp <- read_xml(filename)
  cv_params <- .cvParams(xml_tmp)
  id_refs <- .idRefs(xml_tmp)
  binary_arrays <- .binaryArrays(xml_tmp)

  if(cv_params$name[12] == "64-bit float"){precision = 64}
  if(cv_params$name[12] == "32-bit float"){precision = 32}

  if(cv_params$name[13] == "no compression"){compression = "none"}

  peaks <- lapply(binary_arrays$value,function(x)(.decodePeaks(x,compression = compression, size = precision/8)))
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
  precision <- cv_unique[grep("float", cv_unique$name),]
  inst_model <- cv_unique[grep("serial number", cv_unique$name),]
  compression <- cv_unique[grep("compression", cv_unique$name),]
  schema <- xml_attrs(xml_children(xml_tmp)[[1]])[["schemaLocation"]]
  file_id <- xml_attrs(xml_children(xml_tmp)[[1]])[["id"]]

  meta_data$fileID <- file_id
  meta_data$precision <- precision$name
  meta_data$compressin <- compression$name
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


