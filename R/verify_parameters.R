#' Verify input parameters for the graph
#'
#' Mainly for internal use.
#'
#' @param coord Coordinates of the locations
#' @param type Type of graph
#' @param par Parameter(s) for the graph
#' @param maxR Maximum range for edges, helps in large patterns.
#' @param doDists Precompute distances? Speeds up some graphs, takes up memory.
#' @param preGraph Precomputed graph, taken as a super-graph
#'
#' @export

sg_verify_parameters <- function(coord, type, par, maxR, doDists, preGraph) {
  # which graph
  i<-pmatch(type, names(SG_GRAPH_PARAMETERS))
  if(is.na(i))
    stop(paste0("'", type, "' is not a supported graph type. Pick one from:\n",
                             paste(names(SG_GRAPH_PARAMETERS), collapse=" ") )
           )

  # check par
  par_should_be <- unlist( SG_GRAPH_PARAMETERS[[i]] )

  if(length(par) & length(par_should_be)==0) stop(paste0("'", type, "' does not take parameters."))
  ####################################################################
  # graphs with some parameters
  #
  # special cases with marks:
  marked_ok <-  TRUE
  if(i %in% c(3,4)) { # mass geometric or mark cross
    if(!is.numeric(par) | length(par) != nrow(x)){
      par_should_be <- c(marks=paste0("vector of length ", nrow(x),  " for the marks"))
      marked_ok <- FALSE
    }
  }
  # RST needs coordinates
  if(i == 8) {
    if(!is.numeric(par) | length(par) != ncol(x)){
      par_should_be <- paste0("vector of length ", ncol(x),  " for the center point coordinates.")
      marked_ok <- FALSE
    }
  }
  # CCC
  if(i==10){
    if( !is.factor(par) | length(par) != nrow(x)){
      par_should_be <- paste0("vector of length ", nrow(x),  " for the center point coordinates.")
      marked_ok <- FALSE
    }
    else{
      par <- as.integer(par)
    }
  }


  ################
  # no marks
  if(length(par)!=length(par_should_be) | !marked_ok)
    stop(paste("'",type,"' graph needs par=",
          paste(par_should_be, collapse=","),sep="")
  )
  # check maxR
  if(maxR<0) stop("'maxR' < 0 given.")

  # check doDists

  # check preGraph
  if(!is.null(preGraph)) if(!is(preGraph, "sg")) stop("'preGraph' should be of class 'sg'.")
  # ok
  par
}
