#include "Pp.h"

/********************************************************************************************/
Pp::~Pp()
{
}
/********************************************************************************************/
Pp::Pp(NumericMatrix coord)
{
  X = coord;
  npoints = X.nrow();
  dim = X.ncol();
  // distance
  dist = &Pp::distEuclidian;
}
/********************************************************************************************/
int Pp::size(){
  return npoints;
}
int Pp::d() {
  return dim;
}
/********************************************************************************************/
double Pp::getCoord(int *i, int *d){
  return X(*i, *d);
}

/********************************************************************************************/
double Pp::distEuclidian(int *i, int *j)
{
  if(*i==*j) return 0.0;
  if(*i>*j) return distEuclidian(j, i);
  double s=0;
  for(int k=0; k < dim; k++) s+=pow(getCoord(i, &k)-getCoord(j, &k) , 2.0);
  return sqrt(s);
}
/********************************************************************************************/
double Pp::getDist(int *i, int *j)
{
  return (this->*dist)(i,j);
}
