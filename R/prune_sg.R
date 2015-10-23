#' Prune a graph
prune.sg<-function(g, level=1, verbose=TRUE) {
  if(!is(g,"sg")) stop("g not sg object.")
  if(is.null(level)) return(g)
  if(level<=0)return(g)

  g <- sg2sym(g)

  edges <- prune_c(g$edges, level, verbose)

  as.sg(edges,type=g$type, pars=g$parameters,
        note=c(g$note, paste("pruned with level=", as.integer(level),sep="")))
}
