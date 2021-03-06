% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allGenerics.R, R/createGroup-method.R
\name{createGroup}
\alias{createGroup}
\alias{createGroup,SRM-method}
\title{Create Group}
\usage{
createGroup(object, index, rt, width, id)

\S4method{createGroup}{SRM}(object, index, rt, width, id)
}
\arguments{
\item{object}{a SRM object}

\item{index}{a numeric value of the target transition index}

\item{rt}{a numeric value of target Rt time (in minutes)}

\item{width}{a numeric value of tolerated Rt width (in seconds)}

\item{id}{a character string for peak group identification}
}
\value{
a \code{SRM} object
}
\description{
Create peak group based on user specified retention time
}
