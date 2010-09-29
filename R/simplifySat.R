simplifySat <-
function(satin.obj, extent=1, FUN=mean, lonlim=NULL, latlim=NULL, format="wide")
{
  if ( class(satin.obj) != "satin" )
   stop ( "need object of class 'satin'" )
   itype <- satin.obj$itype
   if ( itype %in% c("avhrr" , "oceancolor") ) {
     x <- satin.obj[[1]]
     y <- satin.obj[[2]]
     z <- satin.obj[[3]]
     period <- satin.obj$period

       if (is.null(lonlim)){
         xs <- seq( floor(min(x)), ceiling(max(x)), extent )
         if (max(x) > xs[length(xs)]) 
           xs <- c(xs, xs[length(xs)] + extent)
        } else {
         xs <- seq(lonlim[1], lonlim[2], extent)
        }

       if (is.null(latlim)){
         ys <- seq( floor(min(y)), ceiling(max(y)), extent )
         if (max(y) > ys[length(ys)]) 
           ys <- c(ys, ys[length(ys)] + extent)
        } else {
         ys <- seq(latlim[1], latlim[2], extent)
        }

     pmx <- xs[-length(xs)] + extent/2
     pmy <- ys[-length(ys)] + extent/2

     nlon <- x
       for (i in 1:length(x)) {
           for(j in 1:length(xs)) {
             if(x[i] >= xs[j] & x[i] < xs[j+1]) 
             nlon[i] <- pmx[j]
            }    
        }     

     nlat <- y
       for (i in 1:length(y)) {
           for(j in 1:length(ys)) {
             if(y[i] >= ys[j] & y[i] < ys[j+1]) 
             nlat[i] <- pmy[j]
            }    
        }     

     pdfx <- data.frame( lon=sort(rep(nlon, length(nlat))),  
                         lat=rep(nlat, length(nlon)), param=as.vector(z) )
     dfx <- pdfx[complete.cases(pdfx), ]
     
       if (format == 'long'){    
         sx <- aggregate(dfx$param, by=list(dfx$lon, dfx$lat), FUN)
         colnames(sx) <- c("longitude", "latitude", "param")
        } else {
         tsx <- tapply(dfx$param, list(dfx$lat, dfx$lon), FUN, na.rm=TRUE)
         dimnames(tsx) <- NULL
         sx <- list(longitude=pmx, latitude=pmy, param=tsx, period=period, itype = itype)
         attr(sx, "class") <- "satin" 
        }
    } else { 
      stop ( "need 'AVHRR' or 'OceanColor' data")
    }
  sx
}

