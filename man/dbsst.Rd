\name{dbsst}
\alias{dbsst}
\docType{data}
\title{
 SST sample data - Pathfinder sensor (AVHRR) 
}
\description{
  Mean Sea Surface Temperature in December 2009 off northwest Mexico 
}
\usage{data(dbsst)}
\format{
A list of 5

 $ longitude: a vector of longitudes (length=569)
 
 $ latitude : a vector of latitudes (length=501) 
 
 $ param      : a matrix [501, 569] of mean SST values
 
 $ period   : character string of the averaging period in the data (Dec 2009)
 
 $ itype    : character string giving the type of data ("avhrr")
  
 - attr(*, "class")= chr "satin"
}
\details{
Source data file (200912.s04m3pfrt-bsst.hdf) was obtained from the link below and converted to hdf v5 with h4h5tools (\url{http://www.hdfgroup.org/h4toh5}) before being processed with the \code{\link{satinGet}} function. The selected area covers from 20 to 42 degrees of latitude North and from 130 to 105 degrees of longitude West.
}
\source{
  \url{http://aspera.jpl.nasa.gov/download/pub/sea_surface_temperature/avhrr/pathfinder/data_v5/monthly/day/04km/2009/bsst/}
}
\references{
  \url{http://podaac.jpl.nasa.gov/DATA_CATALOG/avhrrinfo.html}
}
\examples{
data(dbsst)
str(dbsst)
satinView(dbsst)
}
\keyword{datasets}
