#' Creator for sgc
#'
#' @export
as.sgc<-function(edges, type="?",pars=NULL,note=NULL)
{
  e <- as.sg(edges,type,pars)
  e$parameters<-pars
  e$nclusters<-length(edges)
  e$N<-max(unlist(lapply(edges,max)))
  names(e)[1]<-"clusters"
  class(e)<-"sgc"
  if(!is.null(note))e$note<-note
  e
}
###############################################################################
#' sgc print method
#'
#' @exportMethod print
#' @export
print.sgc<-function(x,...)
{
  nam<-names(x$parameters)
  p<-"?"
  p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")

  cat(paste("'Spatgraphs' cluster/component list-of-lists:",
            "\ngraph type '",x$type,"'",p,", ",x$nclusters," component",ifelse(x$N>1,"s","")," (",x$N," points).\n",sep=""))
  if(!is.null(x$note))cat(paste("Note: ", x$note,".\n",sep=""))
}

#####################################################################################
#' sgc summary
#'
#' @exportMethod summary
#' @export
summary.sgc<-function(object, ...)
{
  args<-list(...)
  print(object)
  cls<-sapply(object$clusters, length)
  cat("Isolated points:",sum(cls==1),"\n")
  cat("Cluster size stats:\n")
  print(summary(cls))
}

