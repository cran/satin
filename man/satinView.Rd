\name{satinView}
\alias{satinView}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Easy maps from satellite data
}
\description{
 Visualisation of satellite data (SST, chlorophyll-a concentration and wind speed) from AVHRR, Aqua Modis, SeaWiFS and QuikSCAT products   
}
\usage{
satinView(satin.obj, xlim = NULL, ylim = NULL, zlim = NULL, xoffs = 0, 
  yoffs = 0, map = NULL, map.col = "grey", map.outline = "black", colimg
  = NULL, colbar = TRUE, colbar.pos = "r", main = NULL, main.pos = "tr", 
  pass = c("mean", "ascending", "descending"), scale = 1, length = 0.05, 
  colarrow = NULL, ra.pos = NULL, ra.speed = NULL, ra.col = "black", 
  add2map = FALSE, log = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{satin.obj}{
 a satin object with satellite data as returned by \code{satinGet}
}
  \item{xlim}{ minimum and maximum longitude values for the map }
  \item{ylim}{ minimum and maximum latitude values for the map }
  \item{zlim }{ minimum and maximum parameter values for the map. Only meaningful when \code{itype="avhrr"} or \code{itype="oceancolor"} }
  \item{xoffs}{ a value to add/substract from \code{xlim} to help centering the map and the image }
  \item{yoffs}{ a value to add/substract from \code{ylim} to help centering the map and the image }
  \item{map}{ a user defined map of class "sp" }
  \item{map.col}{ color for the map }
  \item{map.outline}{ color for the map outline }
  \item{colimg}{ a list with a vector of color names and a vector of color breaks as returned by \code{\link{genColorPal}}. Only meaningful when \code{itype="avhrr"} or \code{itype="oceancolor"} }
  \item{colbar}{ logical. If \code{TRUE} a color bar is added to the plot  }
  \item{colbar.pos}{ position of the color bar. Either top "t" or right "r" side }
  \item{main}{ title for the plot, defaults to \code{period} in \code{satin.obj} }
  \item{main.pos}{ position for the title: top right ("tr"), top left ("tl"), bottom right ("br") or bottom left ("bl")  }
  \item{pass}{ either ascending or descending pass or mean. Only meaningful when \code{itype="quikscat"} }
  \item{scale}{ an scalar for adjusting arrow length. Only meaningful when \code{itype="quikscat"} }
  \item{length}{ an scalar for adjusting arrow head size. Only meaningful when \code{itype="quikscat"} }
  \item{colarrow}{ color(s) for arrows; see details. Only meaningful when \code{itype="quikscat"} }
  \item{ra.pos}{ position (lon, lat) for reference arrow. Only meaningful when \code{itype="quikscat"} }
  \item{ra.speed}{ size (in m/s) for reference arrow. Only meaningful when \code{itype="quikscat"} }
  \item{ra.col}{ color for reference arrow. Only meaningful when \code{itype="quikscat"} }
  \item{add2map}{ a logical value. If \code{FALSE} an entire new plot is created. If \code{TRUE} wind speed arrows are added to an existing plot. Only meaningful when \code{itype="quikscat"} }
  \item{log}{ logical. If \code{TRUE} the logarithm of the parameter is obtained and mapped. This may be useful for chlorophyll concentration data}
  \item{\dots}{ further arguments to pass to \code{plot} }
}
\details{
  The only mandatory argument for this function to produce a map is an object of class "satin" (\code{satin.obj}), which contains the longitude and latitude vectors and the corresponding parameter matrix (SST; chlorophyll concentration) or matrices (\code{x} and \code{y} components of wind speed for the ascending and descending pass and their average). All the other arguments are optional and are conceived to customize the map produced.  For wind speed data (\code{itype} = "quikscat"), the \code{colarrow} argument accepts either a single color or an object produced by \code{\link{genColorArrows}} specyfying a color for every arrow. 
}
\value{
 a map of the corresponding parameter is produced
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{ Héctor Villalobos }
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{satinGet}}%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

  ## SST data (AVHRR)
  data(dbsst)
  satinView(dbsst)
  satinView(dbsst, map=dmap, colbar.pos="t")

  ## Chl-a concentration data (Aqua Modis) in actual units and in logarithmic scale
  data(dchla)
  satinView(dchla, map=dmap, xlim=c(-130, -105), ylim=c(20, 40))
  x11()
  satinView(dchla, map=dmap, xlim=c(-130, -105), ylim=c(20, 40), log = TRUE)  
  
  ## Wind speed data (QuikSCAT) 
  data(dwind)
  satinView(dwind)   

  ## adding a reference arrow
  satinView(dwind, colarrow="blue", ra.pos=c(-110, 40), ra.speed=10)   

  ## arrows in diferent colors
  cp <- genColorPal(mini=0, maxi=22, stp=2)
  ca <- genColorArrows(dwind, "a", cp)
  satinView(dwind, map=dmap, pass="a", ra.pos=c(-110, 40), ra.speed=15, 
    scale=0.9, length=0.1, colarrow=ca, colbar=TRUE)

  ## add arrows to an existing map
  par(mar=c(4, 4, 2, 4))
  plot(dmap, xlim=c(-115, -105), ylim=c(20, 30), col="grey20")
  box(); axis(1); axis(2)
  satinView(dwind, pass="m", scale=1, length=0.1, colarrow=ca, 
        add2map=TRUE, ra.pos=c(-107, 28), ra.speed=10, ra.col="yellow", 
        colbar=TRUE, colbar.pos="r")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
