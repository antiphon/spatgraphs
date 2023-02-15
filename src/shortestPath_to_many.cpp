#include <Rcpp.h>
#include <vector>
#include "spatcluster.h"
using namespace Rcpp;


bool Anypos(IntegerVector b) {
  for(int i = 0; i < b.size(); i++) if(b(i) > 0) return true;
  return false;
}


// for distances
double dist_1(int i, int j, NumericMatrix y){
  return 1.0;
}

double dist_eucl(int i, int j, NumericMatrix y) {
  double d = 0.0;
  for(int z=0;z < y.ncol(); z++ ) d += pow(y(i,z)-y(j,z),2);
  return sqrt(d);
}

// [[Rcpp::export]]
List shortestPath_to_many_djikstra_c(int from,
                                     IntegerVector to_vec, // in base-1
                                     List edges,
                                     NumericMatrix coords) {
  // first, need to cluster.
  // this is the slow bit
  //
  List clusters = spatcluster_c(edges, false);

  // Then  check if we are in the same cluster:
  int i,j;
  List cluster;
  bool found = false;

  // which cluster do we start from
  for(i = 0; i < clusters.size(); i++) {
    cluster = clusters(i);
    for(j = 0; j < cluster.size(); j++) {
      if(cluster(j) == from) {
        found = true;
        break;
      }
    }
    if(found) break;
  }

  // Then we loopdidoop over the targets
  int n_to = to_vec.size();
  int to, toi;

  List paths(n_to);
  NumericVector path_lengths(n_to);
  // go

  for(toi = 0; toi < n_to; toi++) {
    to = to_vec(toi);

    // is target in the same place
    found = false;
    for(i = 0; i < cluster.size(); i++){
      if(cluster(i) == to)
      {
        found = true;
        break;
      }
    }
    if(!found) {
      IntegerVector ij(2);
      ij(0) = from;
      ij(1) = to;
      path_lengths(toi) = R_PosInf;
      paths(toi) = NA_REAL;
      continue;
    }
    // seems that we are within the same cluster
    // sort out the node-distances
    double (*dfun)(int i, int j, NumericMatrix coords);
    dfun = &dist_1;
    if( coords.ncol() > 1 && coords.nrow() > 1 ) dfun = &dist_eucl;

    //
    int nc = cluster.size();
    IntegerVector left(nc, 1);
    double inf = R_PosInf;
    NumericVector dists(nc, inf);
    IntegerVector neighs;
    int u, v, uu = 0, vv, h;

    IntegerVector previous(nc, -1);

    double dx, alt;

    // index of starting node
    for(j = 0;; j++) if(cluster(j) == from) break;
    dists(j) = 0;

    h = 0;

    while( Anypos(left) ) {
      //    Rcpp::checkUserInterrupt();
      // index of node with smallest dist
      dx = inf;
      for(i = 0; i < nc; i++)
        if(left(i)>0 && dists(i) < dx) {
          dx = dists(i);
          uu = i;
        }
        u = cluster(uu);
        neighs = edges(u-1);
        for(i = 0; i < neighs.size(); i++) {
          v = neighs(i);
          alt = dists(uu) + dfun(u-1, v-1, coords);
          // index of neighbour in cluster list
          for(j = 0; j < nc; j++) if(cluster(j) == v) break;
          vv = j;
          if(alt < dists(vv)) {
            dists(vv)    = alt;
            previous(vv) = uu;
          }
          if(v == to) // arrive
            for(j=0; j<nc; j++) left(j) = 0;
        }
        left(uu) = 0;
        h++;
    }

    //  return(previous);
    // reverse travel the path
    for(j = 0;; j++) if( cluster(j) == to ) break;
    vv = j;
    std::vector<int> path;
    while(  previous(vv) > -1 ) {
      path.push_back(cluster(vv));
      vv   = previous(vv);
    }
    path.push_back(from);

    // result
    IntegerVector pathR(path.size());
    dx = 0;
    for(j = 0; j < path.size(); j++) {
      if(j>0) dx += dfun(path.at(j)-1, path.at(j-1)-1, coords);
      pathR(path.size()-j-1) = path.at(j);
    }
  // gather
    path_lengths(toi) = dx;
    paths(toi) = pathR;
  }



  return List::create(Named("d") = path_lengths,
                      Named("paths") = paths);
}

