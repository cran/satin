\name{genColorArrows}
\alias{genColorArrows}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Assigning colors to wind speed arrows
}
\description{
  Generates a vector of color names according to wind speed arrow lengths 
}
\usage{
genColorArrows(satin.obj, pass = c("mean", "ascending", "descending"), pal)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{satin.obj}{
  wind speed data as returned by \code{\link{satinGet}}
}
  \item{pass}{
  ascending, descending or mean
}
  \item{pal}{
  a list with a vector of interpolated colors and a corresponding vector of color breaks, as returned by \code{\link{genColorPal}}
}
}
\details{
  \code{genColorArrows} first calculates wind speeds from \code{u} and \code{v} components of wind speed from the selected pass or the mean, and then accordingly assigns a color from \code{pal}. 
  Resulting object can be used in \code{\link{satinView}} function for drawing wind speed arrows in different colors.
}
\value{
  A list with components
  \item{speed }{a matrix of wind speeds}
  \item{col.arrows }{a vector of color names}
  \item{pal}{color palette used}
  \item{breaks }{a vector of color breaks}
  \item{speed.lims }{the minimum and maximum speed in the data}
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
 \code{\link{satinView}}, \code{\link{genColorPal}}, \code{\link{satinGet}}
}
\examples{
 data(dwind)
 ## Define colors and breaks
 cp <- genColorPal(mini=0, maxi=26, stp=2)
 ## assign colors to arrows
 ca <- genColorArrows(dwind, pass="m", pal=cp)

 satinView(dwind, pass="m", colarrow=ca, colbar=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
