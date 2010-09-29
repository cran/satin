\name{dchla}
\alias{dchla}
\docType{data}
\title{
 Chlorophyll sample data - Aqua Modis sensor
}
\description{
  Mean chlorophyll-a concentration in July 2010 off northwest Mexico
}
\usage{data(dchla)}
\format{
A list of 5

 $ longitude: a vector of longitudes (length=300)
 
 $ latitude : a vector of latitudes (length=264)
 
 $ param    : a matrix [264, 300] of mean Chl-a values 
 
 $ period   : character string of the averaging period in the data (1 - 31 Jul 2010)
 
 $ itype    : character string giving the type of data ("oceancolor")
 
 - attr(*, "class")= chr "satin"
}
\details{
Source data file (A20101822010212.L3m_MO_CHL_chlor_a_9km.bz2) was obtained from the link below, then uncompressed and converted to hdf v5 with h4h5tools (\url{http://www.hdfgroup.org/h4toh5}) before being processed with the \code{\link{satinGet}} function.   The selected area covers from 20 to 42 degrees of latitude North and from 130 to 105 degrees of longitude West.
}
\source{
  \url{http://oceancolor.gsfc.nasa.gov/cgi/l3}
}
\references{
  \url{http://oceancolor.gsfc.nasa.gov/DOCS/publications.html}
}
\examples{
data(dchla)
str(dchla)
satinView(dchla)

## view in logarithmic scale
satinView(dchla, log=TRUE)
}
\keyword{datasets}


