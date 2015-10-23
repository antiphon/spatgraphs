#' plot clusters
#' @param gc cluster object
#' @param x point pattern object used for computing the graph
#' @param add add or plot new
#' @param atleast plot only cluster with 'atleast' points in them
#' @exportMethod plot
#' @export

plot.sgc<-function(gc, x, atleast=2, add=FALSE, col, ...)
{
  x <- sg_parse_coordinates(x)
  w <- (1:gc$nclusters)[sapply(gc$clusters,length)>=atleast]
  n <- length(w)

  if(missing(col)) col <- rgb(red=runif(n,0.1,1),green=runif(n,0.1,1), blue=runif(n,0.1,1) )
  if(ncol(x)==2) {
      if(!add) plot(NA, NA, xlim=range(x[,1]), ylim=range(x[,2]), asp=1, xlab="x", ylab="y")
      for(i in w)
      {
        points(x[gc$clusters[[i]],1], x[gc$clusters[[i]], 2], col=col[i], ...)
      }
  }
  else stop("Plotting of clusters only in 2D")

}
