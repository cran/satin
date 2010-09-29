\name{dmap}
\alias{dmap}
\docType{data}
\title{ Map of northwest Mexico from GSHHS }
\description{
  Map of northwest Mexico obtained from GSHHS database.
}
\usage{data(dmap)}
\details{
  This intermediate resolution map was obtained from the Global Self-consistent, Hierarchical, High-resolution Shoreline Database by means of \code{Rgshhs} function from maptools package.
}
\source{
  \url{ftp://ftp.soest.hawaii.edu/pwessel/gshhs/gshhs_2.1.0.zip}
}
\references{
  Wessel, P., and W. H. F. Smith, A Global Self-consistent, Hierarchical, High-resolution Shoreline Database, J. Geophys. Res., 101, 8741-8743, 1996.
}
\seealso{ \code{\link{Rgshhs}} }
\examples{
  data(dmap)
   plot(dmap, xlim=c(-130, -105) , ylim=c(20, 42), xaxs="i", yaxs="i", 
        axes=TRUE, col="beige", lty=1, border="grey70"); box()
}
\keyword{datasets}
