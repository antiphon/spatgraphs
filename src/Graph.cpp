#include "Graph.h"

#ifndef MAX_DOUBLE
const double MAX_DOUBLE = 9999999;
#endif


/********************************************************************************************/
Graph::~Graph()
{
}
/********************************************************************************************/
Graph::Graph(Pp pp, int type, NumericVector parameters){
  this->pp = &pp;
  this->type = type;
  par = parameters;
  edges.resize(pp.size());
  dbg = 0;
}
/********************************************************************************************/
List Graph::toList() {
  return wrap(edges); // thanks to Rcpp
}

/********************************************************************************************/
void Graph::setdbg(int i) {
  this->dbg = i;
}
/********************************************************************************************/
//The graph methods
/********************************************************************************************/
void Graph::sg_calc()
{
  //start the calculation
       if(type == 1) sg_geometric();
  else if(type == 2) sg_knn();
  else if(type == 3) sg_mass_geometric();
  else if(type == 4) sg_markcross();
  else if(type == 5) sg_gabriel();
  else if(type == 6) sg_MST();
  else if(type == 7) sg_SIG();
  else if(type == 8) sg_RST();
  else if(type == 9) sg_RNG();
  else if(type == 10) sg_CCC();
  if(dbg) Rprintf("\n");
}



/********************************************************************************************/
void Graph::sg_geometric()
{
  sg_geometric(par[0]);
}

void Graph::sg_geometric(double R)
{
  int i,j;
  double dist;
  for(i=0;i<(pp->size()-1);i++)
    for(j=i+1;j<pp->size();j++)
    {
      dist = pp->getDist(&i, &j);
      if(dist < R){
        edges[i].push_back(j+1);
        edges[j].push_back(i+1);
      }
    }
}

/********************************************************************************************/
void Graph::sg_knn() {
  int i,j,l,*k,kk;
  kk = (int) par[0];
  k = &kk;

  if(dbg)Rprintf("%i-nn:", *k);

  double *dists2_i = new double[pp->size()], *dists2_i2 = new double[pp->size()];

  for(i=0;i<pp->size();i++){

      for(j=0;j<pp->size();j++)
        dists2_i2[j]=dists2_i[j]= pp->getDist(&i, &j); //gather the distances to others

      qsort( dists2_i, pp->size(), sizeof(double), compare_doubles); // sort distances, rising

      for(j=1;j<=*k;j++) // find the k nearest
        for(l=0;l<pp->size();l++)
          if( dists2_i[j] == dists2_i2[l] ) //with distance comparison
          {
            edges[i].push_back(l+1);
            break;
          }
    }
  if(dbg)Rprintf(" Ok.");
}


/********************************************************************************************/
void Graph::sg_mass_geometric()
{
  if(dbg)Rprintf("Mass-geometric:");
  int i,j;
  double dist;
  for(i=0;i<pp->size();i++)
    for(j=0;j<pp->size();j++)
    {
      if(i!=j)
      {
        dist = pp->getDist(&i, &j);
        if(dist < par[i]){
          edges[i].push_back(j+1);
        }
      }
    }
    if(dbg)Rprintf(" Ok.");
}
/********************************************************************************************/
void Graph::sg_gabriel()
{
  int kk = (int) par[0];

  if(dbg & (kk>0) ) Rprintf("%i-",kk);
  if(dbg) Rprintf("Gabriel:");

  int dim = pp->d();

  int i,j,k, empty,l;
  double R2, d;
  std::vector<double> center(dim);
  for(i=0;i<(pp->size()-1);i++) {
      for(j=i+1;j<pp->size();j++){
        // center
        for(k=0; k< dim;k++) {
          center.at(k) = fabs(pp->getCoord(&i,&k)-pp->getCoord(&j,&k))/2.0+fmin(pp->getCoord(&i,&k),pp->getCoord(&j,&k));
        }
        R2 = pow(pp->getDist(&i, &j)/2,2);
        //		brute force
        empty = 1+kk;
        for(k=0;k<pp->size();k++)
        {
          if(k != i)
            if( k != j)
            {
              d = 0;
              for(l=0;l<dim;l++) d+= pow(center[l] - pp->getCoord(&k, &l), 2);
              if( d<R2 )
              {
                empty = empty - 1;
                if(empty == 0) break;
              }
            }
        }
        if(empty)
        {
          this->edges[i].push_back(j+1);this->edges[j].push_back(i+1);
        }
      }
    }
  if(dbg)Rprintf(" Ok.");
}

/********************************************************************************************/
void Graph::sg_MST()
{
  if(dbg) Rprintf("MST:");
  int i,j,k=0,l=0,zz,k0=0,l0=0;
  int *done = new int[pp->size()],dn;
  double apu0,apu1,apu2;
  done[0] = 0;
  dn = 1;
  int left=pp->size()-dn;
  while( left > 0 )
  {
    apu2 = MAX_DOUBLE;
    for(i=1; i<pp->size();i++){
      zz = 1;
      apu0=apu2;
      for(j=0; j<dn;j++){
        if(i == done[j] ) {
          zz=0;
          break;
        }
        apu1 = pp->getDist(&i, &done[j]);
        if( apu1<apu0 ){
          apu0=apu1;
          k0=i;
          l0=done[j];
        }
      }
      if(zz){
        if(apu0<apu2){
          apu2=apu0;
          k=k0;
          l=l0;
        }
      }
    }
    done[dn] = k;
    dn++;
    left--;
    this->edges[l].push_back(k+1);
  }
  if(dbg)Rprintf(" Ok.");
}

/********************************************************************************************/
void Graph::sg_markcross()
{
  if(dbg)Rprintf("Markcross: ");
  int i,j;
  double dist;
  for(i=0;i<(pp->size()-1);i++)
    for(j=i+1;j<pp->size();j++)
    {
      dist = pp->getDist(&i, &j);
      if(dist < (par[i]+par[j])){
        edges[i].push_back(j+1);
        edges[j].push_back(i+1);
      }
    }
    if(dbg)Rprintf(" Ok.");
}

/********************************************************************************************/
void Graph::sg_SIG()
{
  if(dbg)Rprintf("Spheres-of-Influence:");
  std::vector<double> pars(pp->size());
  int i,j,dbg0=dbg;
  double dist;
  for(i=0;i<pp->size();i++)
  {
    dist = MAX_DOUBLE;
    for(j=0;j<pp->size();j++)
      if(i!=j) dist = fmin(dist, pp->getDist(&i, &j));
      pars.at(i)=dist;
  }
  dbg=0;
  par = pars;
  sg_markcross();
  dbg=dbg0;
  if(dbg)Rprintf(" Ok.");
}


/********************************************************************************************/
void Graph::sg_RST()
{
  int k;
  int dim = pp->d();
  if(dbg){
    Rprintf("Radial Spanning Tree (o=( ");
    for(k=0; k < dim;k++) Rprintf("%f ", par[k]);
    Rprintf(")):");
  }

  edges.resize(pp->size());
  int i,j,m;
  double apu0,apu1,apu2,apu3;

  for(i=0;i< pp->size(); i++)
  {
    apu0 = 0;
    for(m=0; m <dim ; m++) apu0+=pow(pp->getCoord(&i, &m) - par[m], 2);
    apu0 = sqrt(apu0);
    apu3=MAX_DOUBLE;
    k=-1;
    for(j=0; j < pp->size(); j++)
    {
      if(j!=i)
      {
        apu1 = 0;
        for(m=0; m < dim ; m++) apu1+=pow(pp->getCoord(&j, &m) - par[m], 2);
        apu1 = sqrt(apu1);
        //apu1 = pp->getDist(&j,&foc_i);
        if(apu1 < apu0 )
        {
          apu2 = pp->getDist(&i, &j);
          if( apu2 < apu3 )
          {
            apu3 = apu2;
            k = j;
          }
        }
      }
    }
    if(k>-1) addNew(k, i+1);
  }
  if(dbg) Rprintf(" Ok.");
}

/********************************************************************************************/
void Graph::sg_RNG()
{
  if(dbg) Rprintf("Relative neighbourhood: ");
  int i,j,k,isempty;
  for(i=0;i<(pp->size()-1);i++)
  {
    for(j=i+1;j<pp->size();j++)
    {
      isempty = 1;
      for(k=0;k<pp->size();k++)
        if( (k!=i) & (k!=j) )
          if(pp->getDist(&i,&k) < pp->getDist(&i, &j))
            if(pp->getDist(&j,&k) < pp->getDist(&j,&i))
            {isempty=0;break;}
            if(isempty)
            {
              addNew(i,j+1);
              addNew(j,i+1);
            }
    }
  }
  if(dbg) Rprintf(" Ok.");
}


/********************************************************************************************/
void Graph::sg_CCC()
{
  if(dbg) Rprintf("Class Cover Catch: ");

  double m=MAX_DOUBLE, mm = -MAX_DOUBLE;
  std::vector<double> mass(pp->size());
  int i,j;
  int type0=1; // target type set to 1. Re-arrange in R to change.

  for(i=0; i<pp->size();i++)
  {
    mass.at(i)=mm;
    if(par[i]==type0)
    {
      mass.at(i) = m;
      for(j=0;j<pp->size();j++)
        if( (j!=i) & (par[j]!=type0) )
        {
          mass.at(i) = fmin(mass.at(i), pp->getDist(&i, &j));
        }
    }
  }
  for(i=0;i<pp->size();i++) //TODO: optimize this
    if(par[i]==type0)
      for(j=0;j<pp->size();j++)
        if(i!=j)
          if(par[j]==type0)
            if(pp->getDist(&i, &j)< mass.at(i))
              addNew(i,j+1);

  if(dbg) Rprintf(" Ok.");
}


/********************************************************************************************/
// other stuff
/********************************************************************************************/
void Graph::addNew(int i, int j){ //add j to i's neighbours, no duplication
  int k,isnew=1;
  for(k=0; k< (int) edges.at(i).size();k++)
  {
    if(edges.at(i).at(k) == j)
    {
      isnew=0;
      break;
    }
  }
  if(isnew)
    edges.at(i).push_back(j);
}



/**********************************************************************************/
int compare_doubles(const void *a, const void *b)
{
  const double *da = (const double *) a;
  const double *db = (const double *) b;

  return (*da > *db) - (*da < *db);
}
