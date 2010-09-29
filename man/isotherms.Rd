\name{isotherms}
\alias{isotherms}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Obtaining isotherms from SST data }
\description{
  User defined isotherms are calculated from SST data and returned as polylines
}
\usage{
isotherms(satin.obj, tlevels, plot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{satin.obj}{ 
   a satin object with SST data as returned by \code{\link{satinGet}} 
}
  \item{tlevels}{ vector of desired isotherms  }
  \item{plot}{ logical, if TRUE a plot is produced (the default) }
}
\details{
  Polyline coordinates of desired isotherms are calculated via \code{\link{contourLines}} and \code{\link{convCP}} (from PBSMapping package) 
}
\value{
  a data frame object of class "PolySet". See \code{help(PolySet)} for a complete description
}
\author{ Héctor Villalobos, from code in examples of \code{PBSMapping} package}
\references{
  Schnute, J.T., Boers, N.M., Haigh, R., and Couture-Beil, A. 2008. PBS Mapping 2.57: user's guide revised from Canadian Technical Report of Fisheries and Aquatic Sciences 2549:vi + 118 p. Last updated Jul 24, 2008.
}
\seealso{ \code{\link{contourLines}}, \code{\link{convCP}}, \code{\link{PolySet}} }
\examples{
data(dbsst)
isolin <- isotherms(dbsst, tlevels=c(17, 19, 21))
satinView(dbsst)
addLines(isolin$PolySet, col="white")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line