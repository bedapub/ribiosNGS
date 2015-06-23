#include "algorithm"
#include <Rcpp.h>



typedef std::vector<int> IndexList;
typedef std::vector< std::vector<int> > IndexListSet;

int op_decrease (int i) { return --i;}

double subsetSum(const Rcpp::NumericVector &x,
		 const IndexList &index) {
  int i;
  double stat=0.0;
  for(i=0; i<index.size(); i++) {
    stat += x[ index[i] ]; 
  }
  return(stat);
}

RcppExport SEXP cpp_geneSetPerm(SEXP stats,
				SEXP rinds, // list
				SEXP Nsim) {

#ifdef DEBUG
  // srand(1887);
#else
  // srand(time(NULL));
#endif

  int i,j,k,ng;
  double psum;
  
  Rcpp::NumericVector xx(stats);
  Rcpp::NumericVector xp(Rcpp::clone(stats));
  Rcpp::List rindList(rinds);
  ng = rindList.size();
  std::vector<int> geneSetSizes(ng,0);
  
  IndexListSet indset;
  for(i=0; i<ng;i++) {
    indset.push_back(Rcpp::as< std::vector<int> >(rindList[i]));
    geneSetSizes[i]=indset[i].size();
    transform(indset[i].begin(), 
	      indset[i].end(), 
	      indset[i].begin(), op_decrease);
  }
  int nsim=Rcpp::as<int>(Nsim);

  std::vector<int> scounts(ng,0);
  Rcpp::NumericVector ps(ng);
  Rcpp::NumericVector gsStats(ng);
  
  for(i=0;i<ng;i++) {
    gsStats[i]=subsetSum(xx,indset[i]);
  }


  for(i=0;i<nsim;i++) {
    std::random_shuffle(xp.begin(), xp.end());
    #pragma omp parallel for
    for(j=0; j<ng; j++) {
      psum=subsetSum(xp,indset[j]);
      if(psum>=gsStats[j])
	scounts[j]++;
    }
  }

  for(i=0;i<ng;i++) {
    if(geneSetSizes[i]==0) {
      ps[i]=1.0;
      gsStats[i] = NA_REAL;
    } else {
      ps[i]=(scounts[i]+1.0)/(nsim+1.0);
      gsStats[i] = gsStats[i]/geneSetSizes[i];
    }

  }
  
  Rcpp::DataFrame ret =
    Rcpp::DataFrame::create(Rcpp::Named("mean")=gsStats,
			    Rcpp::Named("geneSetSize")=geneSetSizes,
			    Rcpp::Named("p")=ps);
  
  
  return(ret);
}
