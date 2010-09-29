\name{satin-package}
\alias{satin-package}
\alias{satin}
\docType{package}
\title{
Functions for reading and displaying satellite data for oceanographic applications with R
}
\description{
Main functions extract a user defined subset of satellite data from hdf5 files. Currently supported products includes: AVHRR, QuikSCAT, Aqua MODIS and SeaWiFS. Extracted data (sea surface  temperature, chlorophyll-a concentration and wind speed) can be displayed in maps and is available for further analyses.
}
\details{
\tabular{ll}{
Package: \tab satin\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2010-09-29\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
Two \code{satin} functions allow to extract data from hdf5 files: \code{\link{satinGet}} and \code{\link{satinMGet}}. The first can process one hdf file at a time, while the second is capable of processing several files from the same type. The user must specify the type of data, where type refers to the NOAA product downloaded by the user, and might be one of: "avhrr", "oceancolor" (for both Aqua MODIS and SeaWiFS) or "quikscat". Once the data have been processed, the \code{\link{satinView}} function allows to produce plots with default or user defined colors and maps. Other functions in the package allow to summarize SST or Chl-a data by geographical squares and calculate isotherms. 
}
\author{
Héctor Villalobos and Eduardo González-Rodríguez

Maintainer: Héctor Villalobos <hvillalo@ipn.mx>
}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
