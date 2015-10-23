#' Plot a spatial graph
#'
#' Rudimentary plotting.
#'
#' @param g an 'sg' graph object
#' @param x The point pattern object, same as for computing the 'g'
#' @param which Indices of which out-edges to plot. Default: all
#' @param add Add to existing plot? (default: FALSE)
#' @param addPoints Add points? Will be added if add=FALSE
#' @param points.pch point styling
#' @param points.col point styling
#' @param ponits.cex point styling
#' @param ... passed to 'lines' function
#' @exportMethod plot
#' @export

plot.sg <- function(g, x, which=NULL, add=FALSE,
                    addPoints = FALSE, points.pch=1, points.col=1, points.cex=1,
                    ...) {
  x <- sg_parse_coordinates(x)

  if(is.null(which)) which <- 1:nrow(x)

  if(ncol(x) == 2) {
    if(!add) {
      plot(NA, NA, xlim=range(x[,1]), ylim=range(x[,2]), asp=1, xlab="x", ylab="y")
      addPoints <- TRUE
    }

    # gather edges, could be big
    which <- sort(which)
    e <- g$edges[which]
    nl <- sapply(e, length)
    if(sum(nl)>1e4) stop("can't handle > 10 000 edges.")

    ab <- cbind(rep(which, times=nl[which]), unlist(e))

    # unique edges
    ok <- !duplicated(t(apply(ab, 1, sort)))
    ab <- ab[ok,]
    #
    by_i <- split(data.frame(ab), ab[,1])
    sapply(by_i, function(ab) {
      x0<-  x[ab[1,1],1]
      y0<-  x[ab[1,1],2]
      xo <- x[ab[,2],1]
      yo <- x[ab[,2],2]
      x1 <- as.vector( rbind(x0, xo ))
      y1 <- as.vector( rbind(y0, yo ))
      lines(x1, y1, ...)
    })

    if(addPoints)
      points(x[,1], x[,2], pch=points.pch, col=points.col, cex=points.cex)
  }
  #
  if(ncol(x) == 3) null <- plot3.sg(g, x, which, ...)


}

#' Plot 3d graph
#' @param g sg object
#' @param x coordinates
#' @param which points of which out-edges will be plotted
#' @param ... passed to rgl.lines
#' @import rgl
#' @export
plot3.sg <- function(g, x, which, ...) {

  A <- sg2adj(g)$matrix

  n <- ncol(A)

  which <- sort(which)
  e <- g$edges[which]
  nl <- sapply(e, length)
  if(sum(nl)>1e4) stop("can't handle > 10 000 edges.")

  ab <- cbind(rep(which, times=nl[which]), unlist(e))

  # unique edges
  ok <- !duplicated(t(apply(ab, 1, sort)))
  ab <- ab[ok,]
  #
  by_i <- split(data.frame(ab), ab[,1])
  sapply(by_i, function(ab) {
    x0<-  x[ab[1,1],1]
    y0<-  x[ab[1,1],2]
    z0 <- x[ab[1,1],3]
    xo <- x[ab[,2],1]
    yo <- x[ab[,2],2]
    zo <- x[ab[,2],3]
    x1 <- as.vector( rbind(x0, xo ))
    y1 <- as.vector( rbind(y0, yo ))
    z1 <- as.vector( rbind(z0, zo ))
    rgl.lines(x1, y1, z1, ... )
  })
}
