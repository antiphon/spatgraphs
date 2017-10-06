#include "Pp.h"
#include "Graph.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List spatgraph_c(NumericMatrix coord, int type, NumericVector parameters,
                 double maxR, List preGraph, int verbose) {
  Pp pp(coord);
  Graph graph(&pp, type, parameters, maxR);
  graph.setdbg(verbose);
  graph.set_edges(preGraph);
  graph.sg_calc();
  List edges = graph.toList();
  return edges;
}
