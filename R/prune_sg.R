#' Prune a graph
#'
#' @param g sg object
#' @param level pruning level
#' @param verbose verbosity
#'
#' @details Remove edges from a graph by their path connectivity.
#'
#' @examples
#' x <- matrix(runif(50*2), ncol=2)
#' g <- spatgraph(x, "MST")
#' gp <- prune_sg(g, level = 2)
#' plot(g, x, lty=2)
#' plot(gp, x, add=TRUE, col=2)
#'
#' @export

prune_sg<-function(g, level=1, verbose=FALSE) {
  if(!is(g,"sg")) stop("g not sg object.")
  if(is.null(level)) return(g)
  if(level<=0)return(g)

  g <- sg2sym(g)

  edges <- prune_c(g$edges, level, verbose)

  as.sg(edges,type=g$type, pars=g$parameters,
        note=c(g$note, paste("pruned with level=", as.integer(level),sep="")))
}
