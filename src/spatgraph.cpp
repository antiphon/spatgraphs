#include "Pp.h"
#include "Graph.h"

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List spatgraph_c(NumericMatrix coord, int type, NumericVector parameters, int verbose) {
  Pp pp(coord);
  Graph graph(pp, type, parameters);
  graph.setdbg(verbose);
  graph.sg_calc();
  List edges = graph.toList();
  return edges;
}
