\name{dwind}
\alias{dwind}
\docType{data}
\title{
 Ocean wind sample data - Quikscat sensor 
}
\description{
  Level 3 ocean wind vectors in a 25 km grid, November 20 2009 off northwest Mexico 
}
\usage{data(dwind)}
\format{
A list of 6

 $ longitude: a vector of longitudes (length=100)
 
 $ latitude : a vector of latitudes (length=88)
 
 $ ucomp, $ vcomp : tridimensional arrays [88, 100, 3] of ascending pass, descending pass and  mean  \code{u} and \code{v} components of wind speed 
 
 $ period   : character string of the date for the data (20 Nov 2009)
 
 $ itype    : character string giving the type of data ("quikscat")
 
 - attr(*, "class")= chr "satin"
}
\details{
Source data file (QS_XWGRD3_2009324.20093261702.gz) was obtained from the link below, then uncompressed  and converted to hdf v5 with h4h5tools (\url{http://www.hdfgroup.org/h4toh5}) before being processed with \code{\link{satinGet}} function. The selected area covers from 20 to 42 degrees of latitude North and from 130 to 105 degrees of longitude West.
}
\source{
  \url{ftp://podaac.jpl.nasa.gov/pub/ocean_wind/quikscat/L3/data/2009/}
}
\references{
Perry, K.L., October, 2001. SeaWinds on QuikSCAT Level 3 Daily, Gridded Ocean Wind Vectors (JPL SeaWinds Project). Version 1.1, JPL Document D-20335, Jet Propulsion Laboratory, Pasadena, CA.
}
\examples{
data(dwind)
str(dwind)
satinView(dwind)
}
\keyword{datasets}
