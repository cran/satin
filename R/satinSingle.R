'satinSingle' <- function(satin.Mobj, file)
{
    if ( missing(file) )
      stop ("which file do you want?")
    longit <- satin.Mobj$longitude
    latit <- satin.Mobj$latitude
    avp <- satin.Mobj$period[file]
    itype <- satin.Mobj$itype
    if (itype=="quikscat") {
      ucomp <- satin.Mobj$ucomp[, , , file]
      vcomp <- satin.Mobj$vcomp[, , , file]
      result <- list(longitude = longit, latitude = latit, ucomp = ucomp, 
                     vcomp = vcomp, period = avp, itype = itype)
    } else {
      par.aoi <- satin.Mobj[[3]][ , , file]
      result <- list(longitude = longit, latitude = latit, param = par.aoi, 
                     period = avp, itype = itype)
    }
    attr(result, "class") <- "satin" 
    return(result)
}
