#include <vector>
#include <Rcpp.h>
#include "Pp.h"
using namespace Rcpp;

// [[Rcpp::export]]
List cut_c(List edges, NumericMatrix coord, double R) {
  std::vector<int> onode, *nnode;
  std::vector< std::vector<int> > newgraph;
  Pp pp(coord);

  int i,j,k;

  for(i=0; i < pp.size(); i++) {
    onode = Rcpp::as<std::vector<int> >(edges[i]);
    nnode = new std::vector<int>;
    for(j=0; j < onode.size(); j++) {
      k = onode.at(j)-1;
      if(pp.getDist(&i, &k) < R) {
        nnode->push_back(k+1);
      }
    }
    newgraph.push_back( *nnode );
    delete nnode;
  }
  return wrap(newgraph);
}

