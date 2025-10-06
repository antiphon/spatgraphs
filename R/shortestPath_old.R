#' shortest path on the graph
#'
#' Djikstra's algorithm
#'
#' @param i index from
#' @param j index to
#' @param g sg object
#' @param x optional point pattern from which g was computed
#' @param dbg verbose
#'
#' @details If x is given, we use the point-to-point distances as edge weights. Otherwise, each
#' edge has weight 1.
#'
#' @export

shortestPath_legacy <- function(i, j, g, x=NULL, dbg=FALSE)
{
  if(missing(g))stop("Give a graph g.")
  if(!(i%in%1:g$N)| !(j%in%1:g$N) | i==j) stop("Give i,j different and between 1,...,n.")
  g<-sg2sym(g)
  #
  # Check both are in the same component first.
  e<-spatcluster(g)
  for(k in 1:length(e$clusters))
  {
    if(i%in%e$clusters[[k]]) break;
  }
  # if in different clusters
  if(!(j%in%e$clusters[[k]])) return(list(d=Inf, path=NA));
  #
  # ok something to compute.
  #
  if(is.null(x)) d <- matrix(1, g$N, g$N) - diag(g$N)
  else d <- as.matrix(dist(sg_parse_coordinates(x), upper=TRUE))

  # the relevant subgraph
  cluster <- e$clusters[[k]]
  first   <- g$edges[[i]]
  # in case  neighbours
  if(j %in%first) return(list(d = d[i,j], path = c(i, j)))
  #
  # ok, need to traverse the graph.
  #
  previous  <- rep(NA, length(cluster))
  dists     <- rep(Inf, length(cluster))
  ii        <- which(i==cluster)
  dists[ii] <- 0
  Q         <- cluster
  left      <- rep(TRUE, length(Q))

  while(sum(left)>0)
  {
    u <- which(min(dists[left])==dists)
    u <- u[ceiling(runif(1)*length(u))]
    uu<- cluster[u]

    for(vv in g$edges[[uu]])
    {
      alt = dists[u] + d[uu,vv]
      v <- which(cluster == vv)
      if(alt < dists[v])
      {
        dists[v]   <- alt
        previous[v]<- u
      }
      if(vv == j) left[] <- FALSE  # found
    }
    left[u]<-FALSE
    if(dbg)cat(paste("left: ",sum(left),"\r"))
  }
  if(dbg)cat("\n")

  path <- NULL
  jj   <- which(j==cluster)
  u    <-  jj
  while(!is.na(previous[u]))
  {
    path<- c(path, cluster[u])
    u   <-  previous[u]

  }
  # Done finding the path.
  path<- rev( c(path, i) )
  # Length.
  path_length <- function(path, d){
    S <- 0
    for(i in 2:length(path)) S <- S + d[path[i-1], path[i]]
    return(S)
  }
  # result.
  return(list(d=path_length(path, d), path=path))
}

#EOF


