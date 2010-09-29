isotherms <-
function(satin.obj, tlevels, plot=TRUE)
{
  require(PBSmapping, quietly=TRUE)
  if ( class(satin.obj) != "satin" )
    stop ( "need object of class 'satin'" )
   if ( satin.obj$itype %in% c("avhrr" , "oceancolor") ) {
     GRID <- list(x=satin.obj[[1]], y=satin.obj[[2]], z=t(satin.obj[[3]]))
     CL <- contourLines(GRID, levels=tlevels)
     isoTherm <- convCP(CL)
     attr(isoTherm$PolySet, "projection") <- "LL"
     if (plot == TRUE)
       plotMap(isoTherm$PolySet)
     isoTherm
    } else { 
     stop ( "need 'AVHRR' or 'OceanColor' data")
    }
}

