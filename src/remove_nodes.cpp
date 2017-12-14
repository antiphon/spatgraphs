#include <vector>
#include <Rcpp.h>
#include "Pp.h"
using namespace Rcpp;

// [[Rcpp::export]]
List remove_nodes_sym_c(List edges, IntegerVector set, bool fuse) {
  std::vector<int> onode;
  std::vector< std::vector<int> > newedges;

  int i,j,k;


  for(i=0; i < set.size(); i++){
    j = set(i)-1;
    SEXP ll = edges[j];
    IntegerVector yi(ll);
    for(k=0; k < yi.size(); k++) {
      SEXP lk = edges[yi(k)-1];
      IntegerVector yk(lk);
    }

  }

  return wrap(edges);
}

