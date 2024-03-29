% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/reducePeaks-method.R
\name{reducePeaks}
\alias{reducePeaks}
\alias{reducePeaks,SRM-method}
\title{Reduce Peaks}
\usage{
reducePeaks(object, relative_area)

\S4method{reducePeaks}{SRM}(object, relative_area = 0.05)
}
\arguments{
\item{object}{a SRM object}

\item{relative_area}{a numeric value between 0 and 1, for the threshold of relative peak area}
}
\value{
a \code{SRM} object
}
\description{
Reduce detected peaks using a relative area threshold
}
