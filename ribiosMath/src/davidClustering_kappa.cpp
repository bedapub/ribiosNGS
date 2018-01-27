#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

typedef std::vector<int> IntVec;
typedef std::list< std::vector<int> > IntList;

IntList dc_kappaRowSeeds(NumericMatrix kappaMatrix,
                         double kappaThr = 0.35,
                         int initialGroupMembership = 3,
                         double multiLinkageThr=0.5) {
   int anrow = kappaMatrix.nrow();
   int ancol = kappaMatrix.ncol();
   if(anrow != ancol) {
     Rcpp::stop("The input matrix must be a square matrix, i.e. row numbers must equal to column numbers");
   }

  IntList seeds;
  for(int i=0; i<anrow; i++) {
    IntVec currSeeds;
    for(int j=0; j<ancol; j++)  {
      if(kappaMatrix(i, j) >= kappaThr and i!=j) {
        currSeeds.push_back(j);
       }
    }
    if(currSeeds.size() >= initialGroupMembership-1) {
     if(currSeeds.size() == 1) {
        currSeeds.push_back(i);
        seeds.push_back(currSeeds);
      }  else {
        int overThrKappaCnt = 0;
        for(IntVec::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
          for(IntVec::iterator jt=it+1; jt!=currSeeds.end(); ++jt) {
             if(kappaMatrix(*it, *jt) >= kappaThr) {
                 overThrKappaCnt++;
             }
          }
        }
        int totalKappa = (currSeeds.size()*(currSeeds.size()-1))/2;
        if (overThrKappaCnt >= totalKappa * multiLinkageThr) {
          currSeeds.push_back(i);
          seeds.push_back(currSeeds);
        }
      }
    }
  }
  return(seeds);
}

void dc_uniqueSeeds(IntList &seeds) {
  for(IntList::iterator it=seeds.begin(); it!=seeds.end(); ++it) {
    std::sort(it->begin(), it->end());
    std::transform(it->begin(), it-> end(), 
                   it->begin(), std::bind2nd( std::plus<int>(), 1 ) );
  }
  seeds.sort();
  seeds.erase( std::unique( seeds.begin(), seeds.end() ), seeds.end() );
}

void dc_mergeSeeds(IntList &seeds,
                         double multiLinkageThr=0.5,
                         int mergeRule=1) {
  int lastSeedCount;
  int newSeedCount = -1;
  while(newSeedCount<0 || newSeedCount != lastSeedCount) {
    lastSeedCount = seeds.size();
    bool changed = FALSE;
    for(std::list< std::vector<int> >::iterator it=seeds.begin(); it!=seeds.end(); ++it) {
     for(std::list< std::vector<int> >::iterator jt=it; jt!=seeds.end(); ++jt) {
      if(jt==it) {
        jt++;
        if(jt==seeds.end()) 
          break;
      }
      IntVec seedi = *it;
      IntVec seedj = *jt;
      IntVec intersect;
      IntVec ijunion;
      std::set_intersection(seedi.begin(),seedi.end(),
                            seedj.begin(),seedj.end(),
                            std::back_inserter(intersect));
      std::set_union(seedi.begin(),seedi.end(),
                     seedj.begin(),seedj.end(),
                     std::back_inserter(ijunion));
      int ninter = intersect.size();
      bool toMerge = FALSE;
      if(mergeRule==1) {
         toMerge = ninter >= multiLinkageThr * ijunion.size();
         } else if (mergeRule==2) {
             toMerge = ninter >= multiLinkageThr * seedi.size() && ninter >= multiLinkageThr * seedj.size();
         } else if (mergeRule==3) {
             toMerge = ninter >= multiLinkageThr * seedi.size() || ninter >= multiLinkageThr * seedj.size();
         } else if (mergeRule==4) {
             toMerge = ninter * ninter / (seedi.size() * seedj.size()) >= multiLinkageThr * multiLinkageThr;
         } else {
             Rcpp::stop("should not be here");
         }
         if(toMerge) {
            *it = ijunion;
            seeds.erase(jt);
            changed = TRUE;
            break;
         }
      }
      if(changed) {
        break;
      }
    }
    newSeedCount = seeds.size();
  }
}

//' Cluster rows of a Kappa-statistic-matrix by the hierarhical fuzzy multi-linkage partitioning method proposed by DAVID
//'
//' The function implements the Hierarhical fuzzy multi-linkage partitioning method used in the DAVID Bioinformatics tool.
//'
//' @param kappaMatrix A numeric matrix of Kappa statistics, which is likely returned by \code{\link{rowKappa}} or \code{\link{colKappa}}
//' @param kappaThr Numeric, the threshold of the Kappa statistic, which is used to select initial seeds. Default value: 0.35, as recommended by the authors of the original study based on their experiences.
//' @param initialGroupMembership Integer, the number of minimal members in initial groups. Default value: 3.
//' @param multiLinkageThr Numeric, the minimal linkage between two groups to be merged. Default value: 0.5.
//' @param mergeRule Integer, how two seeds are merged. See below.
//' 
//' Merge rules:
//' \itemize{
//' \item{1: length of intersect divided by length of union no less than \code{multiLinkageThr}.}
//' \item{2: length of intersect divided by length of \emph{both} seeds no less than \code{multiLinkageThr}.}
//' \item{3: length of intersect divided by length of \emph{either} seeds no less than \code{multiLinkageThr}.}
//' \item{4: Geometric mean of length of intersect divided by length of \emph{both} seeds no less than \code{multiLinkageThr}.}
//' }
//' 
//' @author Jitao David Zhang <jitao_david.zhang@roche.com>
//'
//' @note
//' The function has only been tested in a few anecdotal examples. Cautions and more systematic tests are required before it is applied to critical datasets.
//'
//' @references
//' \itemize{
//' \item{Huang *et al.* The DAVID Gene Functional Classification Tool: a novel biological module-centric algorithm to functionally analyze large gene lists. Genome Biology, 2007}
//' \item{Additional file of the manuscript available at \url{https://david.ncifcrf.gov/helps/2D_Introduction_files/additional_file_13.doc}}
//' }
//'
//' @examples 
//' synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
//' rep(0, 4), rep(1, 7), rep(0,4),
//' rep(c(rep(0,5), rep(1,10)), 3),
//' rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
//' rownames(synData) <- sprintf("Gene %s", letters[1:8])
//' colnames(synData) <- sprintf("t%d", 1:15)
//' synKappaMat <- rowKappa(synData)
//' synKappaMat.round2 <- round(synKappaMat, 2)
//' davidClustering_kappa(synKappaMat.round2)
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List davidClustering_kappa(Rcpp::NumericMatrix kappaMatrix,
                                double kappaThr = 0.35,
                                int initialGroupMembership = 3,
                                double multiLinkageThr=0.5,
                                int mergeRule=1) {
  IntList seeds = dc_kappaRowSeeds(kappaMatrix,
                                   kappaThr=kappaThr,
                                   initialGroupMembership=initialGroupMembership,
                                   multiLinkageThr=multiLinkageThr);
  dc_uniqueSeeds(seeds);
  dc_mergeSeeds(seeds);
  return(Rcpp::wrap(seeds));
}
// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
                    rep(0, 4), rep(1, 7), rep(0,4),
                    rep(c(rep(0,5), rep(1,10)), 3),
                    rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
rownames(synData) <- sprintf("Gene %s", letters[1:8])
colnames(synData) <- sprintf("t%d", 1:15)
synKappaMat <- rowKappa(synData)
synKappaMat.round2 <- round(synKappaMat, 2)
davidClustering_kappa(synKappaMat.round2, kappaThr=0.35)
*/
