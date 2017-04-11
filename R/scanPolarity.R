#' Extract scan polarity
#'
#' Extract the scan polarity from a \code{data.frame} of \code{cvParams}
#'
#' @param x a \code{data.frame} of \code{cvParams}
#' @return a vector of scan polarities
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @keywords internal

scanPolarity <- function(x)
  {

  pidx <- which(x[,"accession"] == "MS:1000130" | x[,"accession"] == "MS:1000129")

  pdf <- x[pidx,]

  polarity <- NULL
  for(i in seq_along(pdf[,"accession"])){
    if(pdf[i, "accession"] == "MS:1000129"){
        polarity[i] <- "-"
    }
    if(pdf[i, "accession"] == "MS:1000130"){
      polarity[i] <- "+"
    }
  }

  return(polarity)
  }


