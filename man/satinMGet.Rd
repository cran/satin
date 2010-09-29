\name{satinMGet}
\alias{satinMGet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extracting satellite data from several HDF files at once
}
\description{
  This function is useful when several HDF files must be processed
}
\usage{
satinMGet(noms.hdf5, lats, lons, itype = c("avhrr", "oceancolor", "quikscat"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{noms.hdf5}{ a vector of file names to be processed }
  \item{lats}{ a vector with minimum and maximum latitude }
  \item{lons}{ a vector with minimum and maximum longitude }
  \item{itype}{ a string indicating the type of satellite data (see details)  } 
}
\details{
 This is a convenience function dessigned to process several hdf files at once. The function adds another dimension to \code{param} or \code{ucomp} and \code{vcomp} of the  resulting objects, depending on \code{itype}. This extra dimensions corresponds to the number of files processed. To use these objects with \code{\link{satinView}} function, however, dimensions must be reduced to the single file case. This can be done with  \code{\link{satinSingle}} function (see examples)
}
\value{
An object of class 'satin' with components
  \item{longitude }{a vector of longitudes of the area of interest (aoi)}
  \item{latitude }{a vector of latitudes of the aoi }
  \item{param }{an array of extracted parameter values. The extra dimension accounts for the number of processed files }
  \item{period }{a vector of dates or averaging periods of the processed files }
  \item{itype }{ a string indicating the type of product }
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
 Héctor Villalobos
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{satinGet}};  \code{\link{satinSingle}}
}
\examples{
## Not run: 
# ## directory with 4 AVHRR hdf files 
#    setwd("D:/data")
#    fi <- list.files()
#    lats <- c(20, 25)
#    lons <- c(-115, -110)
#    multiPF <- satinMGet(fi, lats, lons, itype = "avhrr")      
#
# ## mapping the first processed file
#    satinView(satinSingle(multiPF, file = 1)) 
#
#
# ## directory with 3 Quikscat hdf files 
#    setwd("D:/data")
#    fi <- list.files()
#    lats <- c(20, 25)
#    lons <- c(-115, -110)
#    multiQS <- xtSensor(fi, lats, lons, itype = "quikscat")      
#
# ## mapping the second processed file
#    satinView(satinSingle(multiQS, file = 2)) 
## End(Not run)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
