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
  cv_params <- sRm:::cvParams(xml_tmp)
  id_refs <- sRm:::idRefs(xml_tmp)
  binary_arrays <- sRm:::binaryArrays(xml_tmp)

  compidx <- agrep("compression", cv_params$name,max.distance = 0.2)
  comptmp <- unique(cv_params$name[compidx])

  if(comptmp == "no compression"){compression = "none"}
  if(comptmp == "zlib compression"){compression = "gzip"}

  bin_vals <- binary_arrays$value
  bin_prec <- binary_arrays$precision

  bin_df <- data.frame(name = binary_arrays$name, prec = bin_prec)
  bin_df <- unique(bin_df)

  bin_df$prec <- paste(bin_df$prec, "bit float", sep = " ")

  peaks <- list()
  for(i in 1:length(bin_vals)){
    peaks[[i]] <- sRm:::decodePeaks(bin_vals[[i]], compression = compression, size = bin_prec[[i]] / 8)
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

  precision <- NULL
  for(i in 1:nrow(bin_df)){
    precision[[i]] <- paste(bin_df[i,"name"], bin_df[i, "prec"], sep = " : ")
  }

  precision <- gsub("time array", "Time Array", precision)
  precision <- gsub("intensity array", "Intensity Array", precision)
  precision <- gsub("float", "Float", precision)

  inst_serial <- cv_unique[grep("serial number", cv_unique$name),]
  if(nrow(inst_serial) == 0){
    inst_serial <- "NA"
  }else{
    inst_serial <- as.charatcer(inst_serial$value)
  }


  refGroup <- xml_find_all(xml_tmp, "//d1:referenceableParamGroup")
  instrument <- lapply(refGroup, function(x)(xml_attrs(xml_children(x)[[1]])[["name"]]))[[1]]

  xmlUserParam <- xml_find_all(xml_tmp, "//d1:userParam")
  inst_model <- xml_attrs(xmlUserParam)[[1]][["value"]]

  compression <- cv_unique[grep("compression", cv_unique$name),]
  schema <- xml_attrs(xml_children(xml_tmp)[[1]])[["schemaLocation"]]
  file_id <- xml_attrs(xml_children(xml_tmp)[[1]])[["id"]]

  runHeader <- xml_find_all(xml_tmp, "//d1:run")
  acqStamp <- xml_attrs(runHeader)[[1]][["startTimeStamp"]]
  acqDate <- strsplit(acqStamp, "T")[[1]][1]


  meta_data$fileID <- file_id
  meta_data$acqDate <- acqDate
  meta_data$precision <- precision
  meta_data$compressin <- as.character(compression$name)
  meta_data$schema <- schema
  meta_data$instrument <- as.character(instrument)
  meta_data$instrument_model <- as.character(inst_model)
  meta_data$instrument_serial <- inst_serial

  object <- new("SRM")

  object@peaks <- peaks_df[-1]


  object@totalIonCount <- peaks_df[[1]]

  QMZdf <- getQMZs(cv_params)

  polarity <- scanPolarity(cv_params)

  polarity_num <- polarity
  polarity_num <- gsub("\\-", "-1", polarity_num)
  polarity_num <- gsub("\\+", "1", polarity_num)

  object@index <- paste0("Q1: ", QMZdf[,"parent"], " --> ", "Q3: ", QMZdf[,"product"], " (", polarity, ")")

  object@filter <- id_refs[-1]

  object@SHA1 <- as.character(cv_params[which(cv_params$name == "SHA-1"),"value"])
  object@meta <- meta_data

  header <- data.frame(scanIndex = object@index, parent = "", product = "", polarity = "", totalIonCount = "", basePeakInt = "")

  index_clean <- paste0("Q1:", QMZdf[,"parent"], " // ", "Q3:", QMZdf[,"product"])
  header$scanIndex <- index_clean

  names(object@peaks) <- index_clean

  tic <- as.vector(sapply(object@peaks, function(x)(sum(x[,2]))))
  bpi <- as.vector(sapply(object@peaks, function(x)(max(x[,2]))))

  header$parent <- QMZdf[,"parent"]
  header$product <- QMZdf[,"product"]

  header$polarity <- polarity_num

  header$totalIonCount <- tic
  header$basePeakInt <- bpi

  object@header <- header

  return(object)
}
