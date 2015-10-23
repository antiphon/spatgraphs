#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

#ifndef PP_H_
#define PP_H_

class Pp
{
  int npoints;
  int dim;
  NumericMatrix X;
  double (Pp::*dist)(int*, int*);
  std::vector<double> distTriangle;
  std::vector<double> * pdists;

  double distEuclidian(int*, int*);
  double distGreatCircle(int*, int*);
  double distPrecalculated(int*, int*);

  double (Pp::*weight)(int*, int*);
  double weightAll1(int *, int *);
  std::vector<double> weightTriangle;

  std::vector<int> typevec;

public:
  Pp(NumericMatrix );
  virtual ~Pp();

  double getCoord(int *i, int *d);
  int    size();
  int    d();
  double getDist(int *, int *);
  void   setDist(int *, int *, double d);
  void   calcDists();
  void   setDists(double *);

  double getWeight(int *, int *);
  void   setWeight(int *, int *, double d);
  void   calcTransWeights();
  void   setAllTransWeights(double );
};

#endif /*PP_H_*/
