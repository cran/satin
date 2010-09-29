genColorPal <-
function(mini, maxi, stp, colv=NULL)
{
  if (is.null(colv)) 
     colv=c("purple", "blue", "darkblue", "cyan", "green", 
          "darkgreen", "yellow", "orange", "red", "darkred")
  funpal <- colorRampPalette(colv)
  nbcols = length(seq(mini, maxi, stp)) - 1

  result <- list(pal = funpal(nbcols), breaks = seq(mini, maxi, stp))
  result
}

