\name{satinGet}
\alias{satinGet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Extracting satellite data  from an hdf file
}
\description{
 SST, chlorophyll-a concentration or wind speed data for a user defined area is extracted from hdf (v5) files
}
\usage{
satinGet(nom.hdf5, lats, lons, itype = c("avhrr", "oceancolor", "quikscat"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nom.hdf5}{ name of the hdf file }
  \item{lats}{ a vector with minimum and maximum latitude }
  \item{lons}{ a vector with minimum and maximum longitude }
  \item{itype}{ a string indicating the type of satellite data (see details)  }
}
\details{
  This function relies on the \code{hdf5} library, therefore only hdf version 5 files can be read. However, hdf v4 files can be converted to v5 with h4h5tools (\url{http://www.hdfgroup.org/h4toh5}). The user must specify the minimum and maximum latitude and longitude limits within the interval (-90,  90) and (-180, 180) respectively. The \code{itype} argument refers to the satellite product: "avhrr" for SST data from AVHRR (\url{http://aspera.jpl.nasa.gov/download/pub/sea_surface_temperature/avhrr/pathfinder/data_v5/}); "oceancolor" for SST or Chlorophyll-a concentration from both Aqua Modis and SeaWiFS (\url{http://oceancolor.gsfc.nasa.gov/cgi/l3}); "quikscat" for wind speed data from QuikSCAT (\url{ftp://podaac.jpl.nasa.gov/pub/ocean_wind/quikscat/L3/data/}). 
}
\value{
If \code{itype="avhrr" or itype="oceancolor"}, a satin object with components:
  \item{longitude }{a vector of longitudes of the area of interest (aoi)}
  \item{latitude }{a vector of latitudes of the aoi }
  \item{param }{a matrix of extracted SST or Chl-a values }
  \item{period }{date or averaging period }
  \item{itype }{ a string indicating the type of product }

If \code{itype="quikscat"}, instead of \code{param} matrix the following arrays are returned:
  \item{ucomp}{ tridimensional array of ascending pass, descending  
            pass and  mean  \code{u} components of wind speed } 
  \item{vcomp}{ tridimensional array of ascending pass, descending  
            pass and  mean  \code{v} components of wind speed}
}
\author{
 Héctor Villalobos and Eduardo González-Rodríguez
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## Not run:
# sst.pf <- xtAVHRR("200805.s04m3pfrt-bsst.hdf5", 
#                     lats=c(20, 30), lons=c(-130, -105), itype="avhrr")
## End(Not run
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
