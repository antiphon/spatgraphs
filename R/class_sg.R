#' Class creator
#'
#' @export

as.sg <- function(edges=list(), type="?", pars=NULL, note=NULL) {

  e <- list(edges=edges)
  e$N <- length(edges)
  e$symmetric<- "?"
  e$type <- type
  e$parameters <- pars
  if(!is.null(note)) e$note <- note
  class(e)<-"sg"
  e
}

#' verify class sg
#'
#' @export
is_sg <- function(x) if(!is(x,"sg")) stop("input not of class 'sg'.")


###############################################################################
#' print method for sg
#'
#' @exportMethod print
#' @export
print.sg<-function(x,...)
{
  nam <- names(x$parameters)
  p<-"?"
  p<-paste(", par=(",paste(x$parameters,collapse=","),")",sep="")

  cat(paste("'Spatgraphs' edge connection list-of-lists:",
            "\ngraph type '",x$type,"'", p, ", for ", x$N, " points.\n", sep=""))

  if(!is.null(x$note))
    cat(paste("Note: ", x$note,".\n", sep=""))

}


###############################################################################
#' sg summary
#'
#' @exportMethod summary
#' @export
summary.sg<-function(object, ...)
{
  args<-list(...)
  print(object)
  degs<-sapply(object$edges, length)
  cat("Edge count:",sum(degs),"\n");
  cat("Isolated points:",sum(degs==0),"\n")
  #   cat("Symmetric:",object$symmetric,"\n")
  cat("Degree stats:\n")
  print(summary(degs))
  if("pp"%in%names(args))
  {
    l<-edgelengths.sg(object,args$pp)
    cat("Edge length stats:\n")
    print(summary(l$d))
  }
}
