#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

List spatcluster_c(List x, int verbose) {
  std::vector<std::vector<int> > nodelist(x.size()), clustlist;
  // convert to vector

  int i;

  for(i=0; i < x.size(); i ++)
  nodelist.at(i) = Rcpp::as< std::vector<int> > (x[i]);

  clustlist.resize(nodelist.size());
  int n=nodelist.size();
  int *koot = new int[n];
  int j,k;

  if(verbose) Rprintf("Clustering: ");
  for(i=0;i<n;i++) { koot[i]=0;};
  if(verbose) Rprintf("grouping... ");

  for(i=0;i<n;i++)
  {
    clustlist.at(i).clear();
    for(j=0;j<(int)nodelist.at(i).size();j++)//group numbers=indices of neighbours of i //to col(i)
    {
      clustlist.at(i).push_back(nodelist.at(i).at(j)-1);//A[koot[i]*n+i] = j;
    }
  }



  int loop=1;
  i=0;
  int d=0,sizei;
  int l,s;
  int g,sizeg,h;
  if(verbose) Rprintf("sorting... ");
  while(loop)
  {
    sizei = clustlist.at(i).size();
    if((sizei-d)>0)//any unvisited neigh's left
    {
      //			Rprintf("\n %i:%i,%i",i,sizei,d);
      for(k=d;k<sizei;k++)//look through unvisited neigh's in column i
      {
        g=clustlist.at(i).at(k);//A[k*n+i]; //number=index of the neighbour
        //Rprintf("\n  %i:%i,%i",i, k, g);
        sizeg=clustlist.at(g).size();
        for(j=sizeg-1; j >=0 ;j--)//starting union of col(i) U col(g) = col(i)
        {
          s=1;
          h=clustlist.at(g).at(j);//A[j*n+g]; //from rear, for .pop_back()
          if(h != i )
          {
            for(l=0;l<(int)clustlist.at(i).size();l++)//is it new?
            {
              //                             Rprintf(".");
              if(clustlist.at(i).at(l) == h){ s=0; }
            }
            if(s>0) //if new add to col(i)
            {
              clustlist.at(i).push_back(h);
              //Rprintf("(%i)",h);
              //A[koot[i]*n+i]=h;
              //koot[i]++;
              //                             Rprintf("from %i add %i->%i\n",g,h,i);
            }
          }
          clustlist.at(g).pop_back();//A[j*n+g]=-1;
        }//end of union
        clustlist.at(g).clear();//koot[g]=-1; //mark the emptied column
        d++;
      }
    }
    else {
      d=0;
      //if(clustlist.at(i).size()>0)
      clustlist.at(i).push_back(i);
      i++;
    }
    if(i>=n) loop=0;
  }//eo clustering loop

  //  for(i=0;i<n;i++)
  //    if(koot[i]>0){
  //     *clusn=*clusn+1;
  //	 for(j=0;j<koot[i];j++){
  //       for(l=j;l<koot[i];l++){
  //         e[A[j*n+i]*n+A[l*n+i]]=1;
  //         e[A[j*n+i]+A[l*n+i]*n]=1;
  //       }
  //     }
  //    }
  std::vector<std::vector<int> > reslist;
  std::vector<int>  *p;
  for(i=0;i<(int)clustlist.size();i++)//fix the damn index shift
  {
    p = new std::vector<int> ;
    p->resize(0);
    if(clustlist.at(i).size()>0)
    {
      for(j=0;j<(int)clustlist.at(i).size();j++)
      {
        p->push_back(clustlist.at(i).at(j)+1);
        clustlist.at(clustlist.at(i).at(j)).clear();
      }
      reslist.push_back(*p);
    }
  }
  if(verbose) Rprintf("done.\n");
  return wrap(reslist);

}
