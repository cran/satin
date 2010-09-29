\name{simplifySat}
\alias{simplifySat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Summarizing satellite data by quadrants}
\description{
  The \code{simplifySat} function summarize satellite data by quadrants of user defined dimensions 
}
\usage{
simplifySat(satin.obj, extent=1, FUN=mean, lonlim=NULL, latlim=NULL, 
 format="wide")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{satin.obj}{ satellite data object as returned by \code{\link{satinGet}}}
  \item{extent}{ size in degrees of the squared quadrant for the summary }
  \item{FUN}{ summary function, defaults to \code{mean}}
  \item{lonlim}{ x limits for the summary }
  \item{latlim}{ y limits for the summary }
  \item{format}{ Default is \code{"wide"}, producing a satin object suitable for creating a map with \code{\link{satinView}}. When \code{format="long"}, a three column data frame with longitude, latitude and parameter values is produced.  }
}
\value{
If \code{format="wide"}, a satin object with components:
  \item{longitude }{a vector of longitudes of the area of interest (aoi)}
  \item{latitude }{a vector of latitudes of the aoi }
  \item{param }{a matrix of parameter values }
  \item{period}{averaging period}
  \item{itype}{type of data (\code{"avhrr"} or \code{"oceancolor"})}

If \code{format="long"}, a data frame with columns: longitude, latitude and parameter values
}
\author{ Héctor Villalobos }

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
data(dbsst)
sst.1x1 <- simplifySat(dbsst, extent=1) 
satinView(sst.1x1)
}

