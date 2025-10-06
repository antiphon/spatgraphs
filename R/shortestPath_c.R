#' shortest path on the graph
#'
#' Djikstra's algorithm
#'
#' @param i index from
#' @param j index to. Can be a set.
#' @param g sg object
#' @param x optional point pattern from which g was computed
#' @param dbg verbose
#' @param checksym check (and force) symmetry
#'
#' @details If x is given, we use the point-to-point distances as edge weights. Otherwise, each
#' edge has weight 1.
#'
#' @export

shortestPath2 <- function(i, j, g, x = NULL, dbg=FALSE, checksym = TRUE)
{
  if(missing(g)) stop("Give a graph g.")
  if(!(i%in%1:g$N)| !(all(j%in%1:g$N)) | i%in%j) stop("Give i,j different and between 1,...,n.")
  if(checksym) g <- sg2sym(g)
  #
  # Check both are in the same component first.
  # clusters <-spatcluster_c(g$edges, FALSE)
  # for(k in 1:length(clusters)) { # where are we starting from
  #   if(i %in% clusters[[k]]) break;
  # }
  # # if in different clusters
  # if(!(j %in% clusters[[k]])) return(list(d=Inf, path=NA));
  #
  # ok something to compute.
  #
  if(!is.null(x)) {
    y  <- sg_parse_coordinates(x)
  }
  else {
    y <- matrix(NA, 1, 1)
  }
  # the relevant subgraph
  #cluster <- clusters[[k]]
  #res <- shortestPath_djikstra_c(i, j, cluster, g$edges, d)
  res <- shortestPath_to_many_djikstra_c(i, j, g$edges, y)
  res
}

#EOF


