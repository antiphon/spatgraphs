#include <Rcpp.h>
#include <vector>
#include "Pp.h"

using namespace Rcpp;

#ifndef GRAPH_H_
#define GRAPH_H_

class Graph {
  int dbg;
  double maxR;
  bool edges_set;
  Pp *pp; //the point pattern
  std::vector<std::vector<int> > edges; // the edges
  NumericVector par;
  int type;

public:
  Graph(Pp pp, int type, NumericVector parameters, double);
  virtual ~Graph();


  void addNew(int , int); // for those graphs that might have duplicates

  void setdbg(int );

  void sg_calc();

  void sg_geometric();
  void sg_geometric(double );
  void sg_knn();
  void sg_sub_knn();
  void sg_mass_geometric();
  void sg_markcross();
  void sg_gabriel();
  void sg_MST();
  void sg_SIG();
  void sg_RST();
  void sg_RNG();
  void sg_sub_RNG();
  void sg_CCC();

  void set_edges(List);
  void remove_duplicates();

  List toList();
};

int compare_doubles(const void *a, const void *b);

#endif /*GRAPH_H_*/




