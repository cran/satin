satinGet <-
function(nom.hdf5, lats, lons, itype=c("avhrr", "oceancolor", "quikscat"))
{
   queryname <- function(nom, what=c("date", "nomSST", "resolution")) {
     require(chron)
     split_1 <- unlist(strsplit(nom, "\\."))
       if ( what == "date" ) {
           if (substr(nom, 1, 1) %in% c("A", "S")) {
             year <- substr(split_1[1], 2, 5)
             jday <- substr(split_1[1], 6, 8)
             dat <- strptime(paste(year, jday, sep = "-"), "%Y-%j")
             day <- days(dat)
             month <- months(dat, abbreviate = TRUE)
             ncs <- nchar(split_1[1])
             if (ncs == 8) {
                 date.str <- paste(day, month, year, sep = " ")
                } else {
                 beg.date.str <- paste(day, month, sep = " ")
                 end.jday <- substr(split_1[1], 13, 15)
                 end.dat <- strptime(paste(year, end.jday, sep = "-"), "%Y-%j")
                 end.day <- days(end.dat)
                 end.month <- months(end.dat, abbreviate = TRUE)
                 end.date.str <- paste(end.day, end.month, year, sep = " ")
                    if (month == end.month) {
                     date.str <- paste(day, "-", end.day, month, year)
                    } else {
                     date.str <- paste(beg.date.str, end.date.str, sep = " - ")
                    }
                }
            } else {
               if (substr(nom, 1, 1) == "Q") {
                 year <- substr(split_1[1], 11, 14)
                 jday <- substr(split_1[1], 15, 17)
                 dat <- strptime(paste(year, jday, sep = "-"), "%Y-%j")
                 day <- days(dat)
                 month <- months(dat, abbreviate = TRUE)
                 date.str <- paste(day, month, year, sep = " ")
                } else {
                 year <- substr(split_1[1], 1, 4)
                 jday <- substr(split_1[1], 5, 7)
                 dat <- strptime(paste(year, jday, sep = "-"), "%Y-%j")
                 day <- days(dat)
                 month <- months(dat, abbreviate = TRUE)
                 ncs <- nchar(split_1[1])
                 if (ncs == 4) 
                   date.str <- year
                   if (ncs == 6) {
                       if (substr(split_1[2], 4, 4) == "w") {
                         week <- as.numeric(substr(split_1[1], 5, 6))
                         date.str <- paste("Week", week, year, sep = " ")
                        } else {
                         month <- as.numeric(substr(split_1[1], 5, 6))
                         date.str <- paste(month.abb[month], year, sep = " ")
                        }
                    }
                   if (ncs == 7) {
                     date.str <- paste(day, month, year, sep = " ")
                    }
                   if (ncs == 15) {
                     beg.date.str <- paste(day, month, sep = " ")
                     end.jday <- substr(split_1[1], 13, 15)
                     end.dat <- strptime(paste(year, end.jday, sep = "-"), "%Y-%j")
                     end.day <- days(end.dat)
                     end.month <- months(end.dat, abbreviate = TRUE)
                     end.date.str <- paste(end.day, end.month, year, sep = " ")
                       if (month == end.month) {
                         date.str <- paste(day, "-", end.day, month, year)
                        } else {
                         date.str <- paste(beg.date.str, end.date.str, sep = " - ")
                        }
                    }
                }
            }
            return(date.str)
        } else {
           if (what == "nomSST") {
             split_2 <- unlist(strsplit(split_1[2], "-"))
             nel <- length(split_2)
             vari <- split_2[nel]
             return(vari)
            } else {
             split_2 <- unlist(strsplit(split_1[2], "_"))
             nel <- length(split_2)
             resol <- split_2[nel]
             lresol <- nchar(resol)
             if (lresol == 1)
             resol <- paste(resol, "km", sep="")
             return(resol)
            }
        }    
    }    

   require(hdf5)
   avp <- queryname(nom.hdf5, what="date")

   if (itype == "quikscat") {
     hdf5load(nom.hdf5, load = TRUE, verbosity = 0, tidy = TRUE)
     Scale <- attr(asc.avg.wind.vel.u, "scale.factor")
     lims <- c(0.125, 359.875, -89.875, 89.875)
     nbrows = dim(asc.avg.wind.vel.u)[1]
     nbcols = dim(asc.avg.wind.vel.u)[2]
     lon <- seq(lims[1], lims[2], length = nbcols)
     lat <- seq(lims[3], lims[4], length = nbrows)
     coox <- lons
     if (lons[2] <= 0) 
         coox <- 360 + lons
     xlon <- lon >= min(coox) & lon <= max(coox)
     ylat <- lat >= min(lats) & lat <= max(lats)
     longit <- if (lons[2] <= 0) lon[xlon] - 360 else lon[xlon]
     latit <- sort(lat[ylat])
     au <- asc.avg.wind.vel.u[ylat, xlon]
     du <- des.avg.wind.vel.u[ylat, xlon]
     av <- asc.avg.wind.vel.v[ylat, xlon]
     dv <- des.avg.wind.vel.v[ylat, xlon]
     a.count <- asc.wvc.count[ylat, xlon]
     d.count <- des.wvc.count[ylat, xlon]
     au[a.count == 0] <- NA
     au = au * Scale
     du[d.count == 0] <- NA
     du = du * Scale
     av[a.count == 0] <- NA
     av = av * Scale
     dv[d.count == 0] <- NA
     dv = dv * Scale
     u <- matrix(colMeans(rbind(as.vector(au), as.vector(du)), 
         na.rm = TRUE), dim(au))
     v <- matrix(colMeans(rbind(as.vector(av), as.vector(dv)), 
         na.rm = TRUE), dim(av))
     vars <- c("asc.avg.wind.speed", "asc.avg.wind.speed.sq", 
       "asc.avg.wind.vel.u", "asc.avg.wind.vel.v", "asc.rain.flag", 
       "asc.rain.prob", "asc.time.frac", "asc.wvc.count", "des.avg.wind.speed", 
       "des.avg.wind.speed.sq", "des.avg.wind.vel.u", "des.avg.wind.vel.v", 
       "des.rain.flag", "des.rain.prob", "des.time.frac", "des.wvc.count")
     rm(list = vars, pos = 1)
     ucomp <- array(NA, dim = c(length(latit), length(longit), 
         3), dimnames = list(NULL, NULL, c("ascending", "descending", "mean")))
     vcomp <- ucomp
     ucomp[, , 1] <- au
     ucomp[, , 2] <- du
     ucomp[, , 3] <- u
     vcomp[, , 1] <- av
     vcomp[, , 2] <- dv
     vcomp[, , 3] <- v
     result <- list(longitude = longit, latitude = latit, ucomp = ucomp, 
          vcomp = vcomp, period = avp, itype = itype)
    }
    else {
       if (itype == "avhrr") {
         nom.SST <- queryname(nom.hdf5, what="nomSST")
         hdf5load(nom.hdf5, load = TRUE, verbosity = 0, tidy = TRUE)
         slope = attr(get(nom.SST), "dsp.cal.coeffs")[1]
         intercept = attr(get(nom.SST), "dsp.cal.coeffs")[2]
         attach(HDF4.DIMGROUP)
         xlon <- lon >= min(lons) & lon <= max(lons)
         ylat <- lat >= min(lats) & lat <= max(lats)
         longit <- lon[xlon]
         latit <- sort(lat[ylat], decreasing = FALSE)
         sst.aoi <- get(nom.SST)[ylat, xlon]
         sst.aoi = sst.aoi[sort.list(nrow(sst.aoi):1), ]
         sst.aoi = (slope * sst.aoi) + intercept
         sst.aoi[sst.aoi == -3] <- NA
         detach(HDF4.DIMGROUP)
         rm(list = c("HDF4.DIMGROUP", nom.SST), pos = 1)
         result <- list(longitude = longit, latitude = latit, param = sst.aoi, 
            period = avp, itype = itype)
        }
        else {
         hdf5load(nom.hdf5, load = TRUE, verbosity = 0, tidy = TRUE)
         Scaling <- attr(l3m.data, "Scaling")
         Base <- attr(l3m.data, "Base")
         slope <- attr(l3m.data, "Slope")
         intercept <- attr(l3m.data, "Intercept")
         Fill <- attr(l3m.data, "Fill")
         nbrows = dim(l3m.data)[1]
         nbcols = dim(l3m.data)[2]
         resolution <- queryname(nom.hdf5, what="resolution")
          lims4km <- c(-179.97917, 179.97917, -89.979164, 89.979164)
          lims9km <- c(-179.95833, 179.95833, -89.958336, 89.958336)
           if ( resolution == "4km" ) {
             lims <- lims4km
            } else {
             lims <- lims9km
            }
         lon <- seq(lims[1], lims[2], length = nbcols)
         lat <- seq(lims[4], lims[3], length = nbrows)
         xlon <- lon >= min(lons) & lon <= max(lons)
         ylat <- lat >= min(lats) & lat <= max(lats)
         longit <- lon[xlon]
         latit <- sort(lat[ylat])
         par.aoi <- l3m.data[ylat, xlon]
         par.aoi <- par.aoi[sort.list(nrow(par.aoi):1), ]
         par.aoi[par.aoi == Fill] <- NA
           if (Scaling == "linear") {
             par.aoi <- (slope * par.aoi) + intercept
            } else {
             par.aoi <- Base^((slope * par.aoi) + intercept)
            }
         rm(list = "l3m.data", pos = 1)
         if ( exists("l3m.qual", 1) )
           rm(list = "l3m.qual", pos = 1)
         result <- list(longitude = longit, latitude = latit, param = par.aoi, 
               period = avp, itype = itype)
        }
    
    }
    attr(result, "class") <- "satin" 
    return(result)
}

