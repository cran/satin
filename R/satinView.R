satinView <-
function(satin.obj, xlim = NULL, ylim = NULL, zlim = NULL, 
   xoffs = 0, yoffs = 0, map = NULL, map.col = "grey", map.outline = "black",  
   colimg = NULL, colbar = TRUE, colbar.pos = "r", main = NULL, main.pos = "tr",
   pass=c("mean", "ascending", "descending"), scale = 1, length = 0.05, 
   colarrow = NULL, ra.pos = NULL, ra.speed = NULL, ra.col = "black", 
   add2map = FALSE, log = FALSE, ...)
{
  if ( class(satin.obj) != "satin" )
    stop ( "need object of class 'satin'" )
  itype <- satin.obj$itype 
  x <- satin.obj[[1]]
  y <- satin.obj[[2]]
  if ( missing(xlim) ) 
    xlim <- c(floor(min(x)), ceiling(max(x)))
  if ( missing(ylim) ) 
    ylim <- c(floor(min(y)), ceiling(max(y)))

  ## Quikscat #################################################################
   if (itype == "quikscat") { 
       if ( missing(colbar) )
         colbar = FALSE
       xcoor <- matrix(rep(x, length(y)), ncol = length(x), byrow = TRUE)
       ycoor <- matrix(rep(y, length(x)), ncol = length(x), byrow = FALSE)

       if ( missing(pass) )
         pass <- "mean"
         tp <- match.arg( pass )
       switch(tp,
           mean = k <- 3,
           ascending = k <- 1,
           descending = k <- 2)
       u <- satin.obj[[3]][ , , k]
       v <- satin.obj[[4]][ , , k]

       if ( !missing(ra.pos) ) {
         if ( length(ra.pos) != 2) 
           stop("'ra.pos' must contain 'x' and 'y' coordinates for reference arrow")
         ru <- min(which(abs(y - ra.pos[2]) == min(abs(y - ra.pos[2]))))
         cu <- min(which(abs(x - ra.pos[1]) == min(abs(x - ra.pos[1]))))
         if ( missing(ra.speed) ) 
           stop("I need also the speed for reference arrow 'ra.speed'")
         u[ru, cu] <- ra.speed
         v[ru, cu] <- 0
        }
       speed <- sqrt(u^2 + v^2)
       maxspeed <- max(speed, na.rm = TRUE)
       u <- u * scale/maxspeed
       v <- v * scale/maxspeed
       if ( missing(colarrow) ) 
         colarrow = "black"
       if ( length(colarrow) > 1 ) {
         if ( length(colarrow) != 5 ) 
           stop( "'colarrow' must be a single color or a list of length 5" )
         ca <- colarrow
         ca.pal <- ca[[3]]
         ca.breaks <- ca[[4]]
         colarrow <- ca[[2]]
           if ( length(ra.pos) == 2 ) {
             cam <- matrix(colarrow, dim(ca[[1]]))
             cam[ru, cu] <- ra.col
             colarrow <- as.vector(cam)
            }
        }

       if ( add2map == FALSE ) {
           if ( missing(map) ) {
             require(maps)
             map("world", xlim = xlim, ylim = ylim,
               col = map.col, fill = TRUE)
             box(); axis(1); axis(2)
             pu <- par("usr")
               if (colbar == TRUE) {
                 require(plotrix)
                   if (colbar.pos == "r") {
                     color.legend(xl = pu[2], yb = pu[3], xr = pu[2] + 1, yt = pu[4], 
                      rect.col = ca.pal, legend = ca.breaks[2:length(ca.breaks)], 
                      gradient = "y", cex = 0.7, align = "rb")
                    } else {
                     color.legend(xl = pu[1], yb = pu[4], xr = pu[2], yt = pu[4] + 1,
                      rect.col = ca.pal, legend = ca.breaks[2:length(ca.breaks)], 
                      gradient = "x", cex = 0.7, align = "lt")
                    }
                }
            } else {
             require(maptools, quietly=TRUE)
             rimar <- 2.1
             if (colbar == TRUE & colbar.pos == "r")
             rimar <- 4.1
             par(mar = c(5.1, 4.1, 4.1, rimar))
             plot(map, xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i", 
               axes = TRUE, lty = 1, col = map.col, border = map.outline)
             box()
             pu <- par("usr")
               if (colbar == TRUE) {
                 require(plotrix)
                   if (colbar.pos == "r") {
                     color.legend(xl = pu[2], yb = pu[3], xr = pu[2] + 1, yt = pu[4],
                       rect.col = ca.pal, legend = ca.breaks[2:length(ca.breaks)], 
                       gradient = "y", cex = 0.7, align = "rb")
                    } else {
                     color.legend(xl = pu[1], yb = pu[4], xr = pu[2], yt = pu[4] + 1,
                       rect.col = ca.pal, legend = ca.breaks[2:length(ca.breaks)], 
                       gradient = "x", cex = 0.7, align = "lt")
                    }
                }
            }
        }
       par.uin <- function() {
         pu <- par("usr")
         pp <- par("pin")
         c(pp[1]/(pu[2] - pu[1]), pp[2]/(pu[4] - pu[3]))
        }
       arrows(xcoor, ycoor, xcoor + u, ycoor + v, length = length * 
         min(par.uin()), col = colarrow, ...)
       box()
       if (length(ra.pos) == 2) {
         text(x = ra.pos[1], y = ra.pos[2], label = 
         substitute(paste(ra.speed, " ", m.s^{-1}),
         list(ra.speed = ra.speed)), adj = c(0.3, 1.5), 
           cex = 0.7, col = ra.col)
        }
       pu <- par("usr")
       if (colbar == TRUE) {
         require(plotrix)
           if (colbar.pos == "r") {
             color.legend(xl = pu[2], yb = pu[3], xr = pu[2] + 1, yt = pu[4],
               rect.col = ca.pal, legend = ca.breaks[2:length(ca.breaks)], 
               gradient = "y", cex = 0.7, align = "rb")
            } else {
             color.legend(xl = pu[1], yb = pu[4], xr = pu[2], yt = pu[4] + 1, 
               rect.col = ca.pal, legend = ca.breaks[2:length(ca.breaks)], 
               gradient = "x", cex = 0.7, align = "lt")
            }
        }
       if ( missing(main) ) 
         main <- satin.obj$period
       if (main.pos == "tr") {
         cx <- pu[2]
         cy <- pu[4] - abs(abs(pu[4]) - abs(pu[3])) * 0.03
         pos <- 2
        }
       if (main.pos == "br") {
         cx <- pu[2]
         cy <- pu[3] + abs(abs(pu[4]) - abs(pu[3])) * 0.03
         pos <- 2
        }
       if (main.pos == "tl") {
         cx <- pu[1]
         cy <- pu[4] - abs(abs(pu[4]) - abs(pu[3])) * 0.03
         pos <- 4
        }
       if (main.pos == "bl") {
         cx <- pu[1]
         cy <- pu[3] + abs(abs(pu[4]) - abs(pu[3])) * 0.03
         pos <- 4
        }
       text(cx, cy, label = main, pos = pos)
    } else { # Ocean Color ################################################
       z <- satin.obj[[3]]
       if ( log == TRUE ) 
         z <- log(z)
       if ( missing(zlim) ) 
         zlim <- c(floor(min(z, na.rm = TRUE)), ceiling( max(z, na.rm = TRUE)) )
       z[z < zlim[1]] <- zlim[1]
       z[z > zlim[2]] <- zlim[2]
       if ( missing(colimg) ) {
         cb <- genColorPal(mini = zlim[1], maxi = zlim[2], stp = 0.1)
         coli <- cb$pal
         spci <- cb$breaks
        } else {
         coli <- colimg$pal
         spci <- colimg$breaks
        }
       lolim <- c(xlim[1] + xoffs, xlim[2] - xoffs)
       lalim <- c(ylim[1] + yoffs, ylim[2] - yoffs)
       cbrw <- (lolim[2] - lolim[1]) * 0.1
       cbtw <- (lalim[2] - lalim[1]) * 0.1
       if ( missing(map) ) {
         require(maps)
         map("world", xlim = lolim, ylim = lalim)
         pu <- par("usr")
         image(x, y, t(z), xlim = c(pu[1], pu[2]), ylim = c(pu[3], 
            pu[4]), zlim, col = coli, breaks = spci, xaxt = "n", 
            yaxt = "n", add = TRUE)
         map("world", xlim = lolim, ylim = lalim, col = map.col, 
            fill = TRUE, add = TRUE, ...)
          if ( missing(main) ) 
            main <- satin.obj[[4]]
           if (main.pos == "tr") {
             cx <- pu[2]
             cy <- pu[4] - abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 2
            }
           if (main.pos == "br") {
             cx <- pu[2]
             cy <- pu[3] + abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 2
            }
           if (main.pos == "tl") {
             cx <- pu[1]
             cy <- pu[4] - abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 4
            }
           if (main.pos == "bl") {
             cx <- pu[1]
             cy <- pu[3] + abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 4
            }
           text(cx, cy, label = main, pos = pos)
           box(); axis(1); axis(2)
           if (colbar == TRUE) {
             leg <- seq(zlim[1], zlim[2], 2)
             if (zlim[2] > max(leg)) 
              leg <- c(leg, " ")
             require(plotrix)
               if (colbar.pos == "r") {
                 color.legend(xl = pu[2], yb = pu[3], xr = pu[2] + 
                 cbrw, yt = pu[4], rect.col = coli, legend = leg, 
                 gradient = "y", cex = 0.7, align = "rb")
                } else {
                 color.legend(xl = pu[1], yb = pu[4], xr = pu[2], 
                 yt = pu[4] + cbrw, rect.col = coli, legend = leg, 
                 gradient = "x", cex = 0.7, align = "lt")
                }
            }
        } else {
           require(maptools, quietly=TRUE)
           rimar <- 2.1
           if (colbar == TRUE & colbar.pos == "r")
             rimar <- 4.1
           par(mar = c(5.1, 4.1, 4.1, rimar))
           plot(map, xlim = lolim, ylim = lalim, xaxs = "i", yaxs = "i", 
             axes = FALSE, lty = 0)
           pu <- par("usr")
           image(x, y, t(z), xlim = c(pu[1], pu[2]), ylim = c(pu[3], 
             pu[4]), zlim, col = coli, breaks = spci, xaxt = "n", 
             yaxt = "n", add = TRUE)
           par(new = TRUE)
           plot(map, xlim = lolim, ylim = lalim, xaxs = "i", yaxs = "i", 
             axes = TRUE, lty = 1, col = map.col, border = map.outline, ...)
           if ( missing(main) ) 
            main <- satin.obj[[4]]
           if (main.pos == "tr") {
             cx <- pu[2]
             cy <- pu[4] - abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 2
            }
           if (main.pos == "br") {
             cx <- pu[2]
             cy <- pu[3] + abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 2
            }
            if (main.pos == "tl") {
            cx <- pu[1]
             cy <- pu[4] - abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 4
            }
           if (main.pos == "bl") {
             cx <- pu[1]
             cy <- pu[3] + abs(abs(pu[4]) - abs(pu[3])) * 0.03
             pos <- 4
            }
           text(cx, cy, label = main, pos = pos)
           box()
            if (colbar == TRUE) {
             leg <- seq(zlim[1], zlim[2], 2)
             if (zlim[2] > max(leg)) 
               leg <- c(leg, " ")
             require(plotrix)
               if (colbar.pos == "r") {
                 color.legend(xl = pu[2], yb = pu[3], xr = pu[2] + 
                 cbrw, yt = pu[4], rect.col = coli, legend = leg, 
                 gradient = "y", cex = 0.7, align = "rb")
                } else {
                 color.legend(xl = pu[1], yb = pu[4], xr = pu[2], 
                  yt = pu[4] + cbtw, rect.col = coli, legend = leg, 
                  gradient = "x", cex = 0.7, align = "lt")
                }
            }
        }
    }
}

