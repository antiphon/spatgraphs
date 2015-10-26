#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List prune_c(List edges, int level, int verbose) {

  std::vector< std::vector<int> > nodelist;
  int dbg = verbose;
  int i, leaf, count=0, prev, next;
  std::vector<int> left;
  std::vector<int> branch, *pnode;
  left.resize(0);
  branch.resize(0);
  nodelist.resize(edges.size());
  // need to clone the list to a vectorvector
  for(i=0; i < edges.size(); i++){
    nodelist.at(i) = Rcpp::as< std::vector<int> > (edges[i]);
  }


  for(i=0; i < (int)nodelist.size(); i++ ) // get the leaves
  {
    if( (int)nodelist.at(i).size() == 1 )
      left.push_back(i+1);
  }
  if(dbg) Rprintf("found %i leaves, pruning...",(int)left.size());

  while(!left.empty()) // go each branch trough starting from the leaf
  {
    leaf = left.back();
    branch.push_back(leaf);
    prev = leaf;
    next = nodelist.at(leaf-1).at(0);
    while((int) nodelist.at(next-1).size()==2)
    {
      branch.push_back(next);
      if(nodelist.at(next-1).at(0) != prev)
      {
        prev = next;
        next = nodelist.at(next-1).at(0);
      }
      else
      {
        prev = next;
        next = nodelist.at(next-1).at(1);
      }
    }
    if((int)branch.size() <= level) // if short enough branch, cut it.
    {
      pnode = new std::vector<int>;
      pnode->resize(0);
      for(i=0; i < (int)branch.size();i++)
        nodelist.at(branch.at(i)-1).clear();

      for(i=0; i < (int)nodelist.at(next-1).size();i++)
        if(nodelist.at(next-1).at(i) != prev)
          pnode->push_back(nodelist.at(next-1).at(i));
        nodelist.at(next-1).swap(*pnode);
        delete pnode;
        count++;
    }
    left.pop_back();
    branch.clear();
    branch.resize(0);
  }
  return wrap(nodelist);
}

