#' spectral clustering
#'
#' @param g sg object. Should be weighted (with weight.sg-function)
#' @param m levels to consider
#' @param K number of assumed clusters
#'
#' @export

spectral.sg<-function(g, m=2, K=3, ...) {

  if(!is(g, "sg")) stop("g not of class sg.")
  if(is.null(g$weights)) stop("No weights in x. Run weight.sg-function.")

  W <- sg2wadj(g)$matrix
  G <- diag(rowSums(W))
  L <- G-W # Laplacian

  E <- eigen(L)
  l <- E$values
  chosen <- order(l)[1:m+1] # drop first 0 value
  v <- E$vectors[,chosen]
  labels<-kmeans(v, K)$cluster
  #'
  note<-paste("K-means from weighted", g$type, "(", g$par,") note:", g$note)
  #
  list(id=labels,
       sgc=as.sgc(split(1:g$N,labels), type="spectral clustering", pars=list(m=m, K=K), note=note),
       v=v, l=l,
       N = g$N)
}

#' plot spectral clustering results
#'
#' @export
plot.spectral <- function(sg, x) {

  if(missing(x)) stop("Can't plot without x.")

  x <- sg_parse_coordinates(x)

  if(ncol(x)>2) stop("plot only for 2d.")

  par(mfrow=c(1,3))

  plot(sg$sgc, x, col=sg$id, main="Identified clusters", pch=19)

  plot(1:20, sort(sg$l)[1:20], main="20 smallest eigenvalues", xlab="Number", ylab="eigenvalue", col="darkgreen", pch=19)
  abline(h=0, col="gray60", lty=2)

  m <- sg$sgc$par$m
  plot(NA, NA, xlab="index", ylab="", xlim=c(0, sg$N), ylim=c(0, m), yaxt="n", main="Smallest eigenvectors (not in scale)")
  axis(2, at=1:m-0.5, paste("eigen",1:m+1, sep=""), tick=FALSE)

  for(i in 1:m){
    vi<-sg$v[,i]-mean(sg$v[,i])
    vi<-vi/(1.1*m*max(abs(vi)))+i-0.5
    points(1:sg$N, vi, col=sg$id)
  }

  abline(h=c(1:(m-1)))
  abline(h=1:m-.5, col="gray50", lty=2)

}
