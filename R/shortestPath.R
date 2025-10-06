#' shortest path on the graph
#'
#' Djikstra's algorithm
#'
#' @param i index from
#' @param j index to
#' @param g sg object
#' @param x optional point pattern from which g was computed
#' @param dbg verbose
#' @param checksym check (and force) symmetry
#'
#' @details If x is given, we use the point-to-point distances as edge weights. Otherwise, each
#' edge has weight 1.
#'
#' @export

shortestPath <- function(i, j, g, x=NULL, dbg=FALSE, checksym = TRUE)
{
  if(missing(g)) stop("Give a graph g.")
  if(!(i%in%1:g$N)| !(j%in%1:g$N) | i==j) stop("Give i,j different and between 1,...,n.")
  if(checksym) g <- sg2sym(g)
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
  if(is.null(x)) {
    d <- matrix(1, g$N, g$N)
    diag(d) <- 0
  }
  else d <- as.matrix(dist(sg_parse_coordinates(x), upper=TRUE))

  # the relevant subgraph
  cluster <- e$clusters[[k]]
  first   <- g$edges[[i]]
  # in case  neighbours
  if(j %in%first) return(list(d = d[i,j], path = c(i, j)))
  #
  # ok, need to traverse the graph.
  #
  # recalculate the indices
  nc        <- length(cluster)
  cluster0  <- 1:nc
  idx       <- 1:nc
  ii        <- match(i, cluster)
  previous  <- rep(NA, nc)
  dists     <- rep(Inf, nc)
  dists[ii] <- 0
  left      <- rep(TRUE, nc)

  while(any(left)) {
    mdi <- which.min(dists[left])
    uu <- idx[left][mdi]
    # if many same at same distance, sample one.
    if(length(uu) > 1)  uu <- uu[ceiling(runif(1)*length(uu))]
    u <- cluster[uu]
    for(v in g$edges[[u]])
    {
      alt  <- dists[uu] + d[u, v]
      vv   <- match(v, cluster)
      if(alt < dists[vv])
      {
        dists[vv]   <- alt
        previous[vv]<- uu
      }
      if(v == j) left[] <- FALSE  # found
    }
    left[uu] <- FALSE
    if(dbg)cat(paste("left: ",sum(left),"\r"))
  }
  if(dbg)cat("\n")

  path <- NULL
  jj   <- match(j, cluster)
  uu    <-  jj
  while(!is.na(previous[uu]))
  {
    path<- c(path, cluster[uu])
    uu   <-  previous[uu]

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


