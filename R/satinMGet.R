satinMGet <-
function (noms.hdf5, lats, lons, itype=c("avhrr", "oceancolor", "quikscat")) 
{
  fn <- length(noms.hdf5)
   if (itype == "quikscat") {
       for (i in 1:fn) {
         assign( noms.hdf5[i], satinGet(noms.hdf5[i], lats, 
         lons, itype = "quikscat") )
        }
       longitude <- get(noms.hdf5[1])$longitude
       latitude <- get(noms.hdf5[1])$latitude
       period <- rep(NA, fn)
       ucomp <- array(NA, dim = c(length(latitude), length(longitude), 
            3, fn), dimnames = list(NULL, NULL, c("ascending", 
            "descending", "mean"), paste("file", 1:fn, sep = "_")))
       vcomp <- ucomp
       for (j in 1:fn) {
         ucomp[, , , j] <- get(noms.hdf5[j])$ucomp
         vcomp[, , , j] <- get(noms.hdf5[j])$vcomp
         period[j] <- get(noms.hdf5[j])$period
        }
       result <- list(longitude = longitude, latitude = latitude, 
            ucomp = ucomp, vcomp = vcomp, period = period, itype = itype)
    } else {
       for (i in 1:fn) {
         assign(noms.hdf5[i], satinGet(noms.hdf5[i], lats, lons, itype = itype))
        }
       longitude <- get(noms.hdf5[1])$longitude
       latitude <- get(noms.hdf5[1])$latitude
       param <- array(NA, dim = c(length(latitude), length(longitude), fn))
       period <- rep(NA, fn)
       for (j in 1:fn) {
         param[, , j] <- get(noms.hdf5[j])[[3]]
         period[j] <- get(noms.hdf5[j])[[4]]
        }
       result <- list(longitude = longitude, latitude = latitude, 
            param = param, period = period, itype = itype)
    }
  result
}

