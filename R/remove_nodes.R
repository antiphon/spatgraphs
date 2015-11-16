#' Remove edges connected to certain nodes
#'
#' Use at your own risk.
#'
#' @param g sg object
#' @param i indices of nodes for which to remove the edges
#' @param fuse Should the neighours of removed nodes be connected?
#'
#' @details
#' Basically, just clear the neighbourhood of selected indices. If fuse=TRUE,
#' connect neighbours together (excluding i's). Should work over several remove nodes
#' along a path.
#'
#' Note: g should be symmetric. use sg2sym to force symmetry, it is not checked.
#' @example
#'
#' x <- matrix(runif(200), ncol=2)
#' g <- spatgraph(x, "RST", c(1,0))
#' g <- sg2sym(g)
#' i <- sample(100, 50)
#' k <- setdiff(1:100, i)
#' gs <- remove_nodes(g, i, fuse=TRUE)
#' plot(g,x, add=F)
#' points(x[k,], pch=19, col=4)
#' plot(gs, x, add=T, lty=2, col=3)
#'
#' @export

remove_nodes <- function(g, i, fuse = FALSE) {
  is_sg(g)
  if(any(!i%in%(1:g$N))) stop( paste0("indices should be between 1 and ", g$N) )

  edges <- g$edges
  ii <- i
  ni <- length(i)
  for(l in 1:ni) {
    i <- ii[l]
    ni <- edges[[i]]
    for(j in ni){
      edges[[j]] <- setdiff(edges[[j]], i)
      if(fuse) edges[[j]] <- setdiff( union( edges[[j]], setdiff(ni, ii[1:l]) ), j )
    }
    edges[[i]] <- integer(0)
  }
  g$edges <- edges
  g$note <- paste(g$note, "Some nodes have their neighbourhoods cleared", sep="\n")
  g
}


