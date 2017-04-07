#' Extract Q1/Q3 masses
#'
#' Extract parent (Q1) and product (Q3) masses from a \code{data.frame} of \code{cvParams}
#'
#' @param x a \code{data.frame} of \code{cvParams}
#' @return a \code{data.frame} parent and product masses for each SRM transition
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal


getQMZs <- function(x)
  {

  qidx <- which(x[,"accession"] == "MS:1000827")

  qdf <- x[qidx,]

  qseq <- rep(1:(nrow(qdf)/2), each = 2)

  qsplit <- split(qdf, f = qseq)

  parentMz <- productMz <- NULL
  for(i in seq_along(qsplit)){
    parentMz[[i]] <- as.numeric(as.character(qsplit[[i]][1,"value"]))
    productMz[[i]] <- as.numeric(as.character(qsplit[[i]][2,"value"]))
  }

  QMZdf <- data.frame(parent = parentMz, product = productMz)

  if(any(QMZdf[,"product"] > QMZdf[,"parent"]) == TRUE){
    stop("error")
  }

  return(QMZdf)
  }

