#' Remove edges connected to certain nodes
#'
#' Remove the existence of particular nodes from the graph.
#'
#' @param g sg object
#' @param i indices of nodes for which to remove the edges
#' @param fuse Should the neighours of removed nodes be connected?
#' @param verb verbose?
#' @details
#' Basically, just clear the neighbourhood of selected indices. If fuse=TRUE,
#' connect neighbours together (excluding i's). Should work over several remove nodes
#' along a path.
#'
#' Note: g should be symmetric. use sg2sym to force symmetry, it is not checked.
#'
#' Warning: In development.
#'
#' @examples
#'
#' x <- matrix(runif(200), ncol=2)
#' g <- spatgraph(x, "RST", c(1,0))
#' g <- sg2sym(g)
#' i <- sample(100, 50)
#' k <- setdiff(1:100, i)
#' gs <- remove_nodes(g, i, fuse=TRUE)
#' plot(g,x, add=FALSE)
#' points(x[k,], pch=19, col=4)
#' plot(gs, x, add=TRUE, lty=2, col=3)
#'
#' @export

remove_nodes <- function(g, i, fuse = FALSE, verb = FALSE) {
  is_sg(g)
  if(any(!i%in%(1:g$N))) stop( paste0("indices should be between 1 and ", g$N) )
  cat2 <- if(verb) cat else function(...) NULL
  edges <- g$edges
  ii <- i
  Ni <- length(i)

  E <- max(Ni, round(Ni/100))
#   timer <- looptimer(n = Ni)
#   it <- 0
  for(l in 1:Ni) {
    i <- ii[l]
    ni <- edges[[i]]
    for(j in ni){
      edges[[j]] <- setdiff(edges[[j]], i)
      if(fuse)
        edges[[j]] <- setdiff( union( edges[[j]], setdiff(ni, ii[1:l]) ), j )
    }
    edges[[i]] <- integer(0)
    if(l %% E==0) cat2("     \r", round(100*l/Ni), "%")
  }
  cat2("\n")
  g$edges <- edges
  g$note <- paste(g$note, "Some nodes have their neighbourhoods cleared", sep="\n")
  g
}



