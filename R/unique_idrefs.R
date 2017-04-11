#' Parse idRef
#'
#' Parse \code{idRefs} when dynamic acqusition has been used. Where dynamic acquistion is used, each SRM transition
#' contains at retention window aswell as a pair of parent (Q1) and product (Q3) masses. \code{unique_idrefs} parses the
#' \code{idRefs} to extract all information needed to identify a unique SRM-MS transition
#'
#' @param object a \code{SRM} object
#' @return a character vector of parsed \code{idRefs}
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal

unique_idrefs <- function(object)
{

  if(class(object) != "SRM"){
    stop(deparse(substitute(object)), " must be a SRM object", call. = FALSE)
  }

  if(length(object@filter) == 0){
    stop("filter (idRefs) slot is empty", call. = FALSE)
  }

  scan_idrefs <- NULL
  for(i in seq_along(object@filter)){
    scan_idrefs[[i]] <- scan(text = object@filter[[i]], what = "", quiet = TRUE)
  }

  Q1 <- Q3 <- St <- En <- NULL
  for(i in seq_along(scan_idrefs)){
    Q1[[i]] <- scan_idrefs[[i]][grep("Q1", scan_idrefs[[i]])]
    Q3[[i]] <- scan_idrefs[[i]][grep("Q3", scan_idrefs[[i]])]
    St[[i]] <- scan_idrefs[[i]][grep("start", scan_idrefs[[i]])]
    En[[i]] <- scan_idrefs[[i]][grep("end", scan_idrefs[[i]])]
  }


  Q1 <- gsub("Q1=", "", Q1)
  Q3 <- gsub("Q3=", "", Q3)
  St <- gsub("start=", "", St)
  En <- gsub("end=", "", En)

  Q1 <- as.numeric(Q1)
  Q3 <- as.numeric(Q3)

  St <- round(as.numeric(St), digits = 1)
  En <- round(as.numeric(En), digits = 1)

  unique_idref_string <- paste0("Q1 = ", Q1, " // St = ",St, " // En = ", En)

  return(unique_idref_string)
}




