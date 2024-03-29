% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internals.R
\name{centWave}
\alias{centWave}
\title{Wraper for xcms centWave peak detection}
\usage{
centWave(rt, int, snthresh, peakwidth)
}
\arguments{
\item{rt}{a numeric vector of chromatogram retention time in minutes}

\item{int}{a numeric vector of chromatogram intensity}

\item{snthresh}{a numeric value for the Signal-to-Noise cut off to use during peak detection}

\item{peakwidth}{a numeric value for the minimum and maximum tolerated peak width.}
}
\value{
a \code{tibble} of detected peaks
}
\description{
Wraper for xcms centWave peak detection
}
\keyword{internal}
