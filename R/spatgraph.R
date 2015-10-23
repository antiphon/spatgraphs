#' Compute the edges of a spatial graph
#'
#' Given a spatial point pattern, we compute the edges of a graph (network) for a specified type of edge relationship.
#'
#' @param x Input point pattern object
#' @param type Type of the graph
#' @param par Parameter(s) for the graph
#' @param verbose Print details
#' @param maxR Maximum range for edges, helps in large patterns.
#' @param doDists Precompute distances? Speeds up some graphs, takes up memory.
#' @param preGraph Precomputed graph, taken as a super-graph
#'
#' @details
#' Several edge definitions are supported:
#'
#' \describe{
#' \item{geometric}{par=numeric>0. Geometric graph, par = connection radius.}
#' \item{knn}{par=integer>0. k-nearest neighbours graph, par = k.}
#' \item{mass_geometric}{Connect two points if ||x-y||<m(x). par=vector giving the m(x_i)'s}
#' \item{markcross}{Connect two points if ||x-y||<m(x)+m(y). par = vector giving the m(x_i)'s}
#' \item{gabriel}{Gabriel graph. Additional parameter for allowing \code{par=k} instead of 0 points in the circle.}
#' \item{MST}{Minimal spanning tree.}
#' \item{SIG}{Spheres of Influence.}
#' \item{RST}{Radial spanning tree, par=origin of radiation, coordinate vector}
#' \item{RNG}{Relative neighbourhood graph}
#' \item{CCC}{Class-Cover-Catch, par=factor vector of point types. The factor vector is converted to integers according to R's internal representation of factors, and the points with type 1 will be the target. Use \link{relevel} to change the target.}
#'
#' }
#'
#' Voronoi diagram aka Delaunay triangulation is not supported as other R-packages can do it, see. e.g. \link{deldir}.
#'
#' @useDynLib spatgraphs2
#' @export

spatgraph <- function(x, type="geometric", par=NULL, verbose = TRUE,
                      maxR = 0, doDists=FALSE, preGraph = NULL) {

  note <- NULL
  ###########################
  # check inputs:
  #
  # locations
  coord <- sg_parse_coordinates(x, verbose)

  # Verify parameters
  par <- sg_verify_parameters(coords, type, par, maxR, doDists,preGraph)

  if(maxR>0) note <- paste("Precalculated geometric graph with R=", maxR, sep="")

  if(!is.null(preGraph)) {
    note<-paste("'preGraph' given (", preGraph$type, ", par=",paste(preGraph$parameters, collapse=","),")",sep="")
  }
  # clash
  if(maxR>0 & !is.null(preGraph)) stop("Can not handle both 'maxR' and 'preGraph'.")

  typei <- pmatch(type, names(SG_GRAPH_PARAMETERS))
  ###########################
  # compute.
  edges <- spatgraph_c(coord, typei, as.numeric(par), as.integer(verbose))


  ##########################
  # compile result
  as.sg(edges, type=type, pars=par, note=note)
}
