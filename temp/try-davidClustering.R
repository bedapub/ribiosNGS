library(Rcpp)
library(rbenchmark)
library(testthat)
library(ribiosMath)

cppFunction('
Rcpp::List davidClustering_cpp_R(Rcpp::NumericMatrix kappaMatrix,
                               double kappaThr = 0.35,
                               int initialGroupMembership = 3,
                               double multiLinkageThr=0.5) {
  int anrow = kappaMatrix.nrow();
  int ancol = kappaMatrix.ncol();

  std::vector< std::set<int> > seeds;
  for(int i=0; i<anrow; i++) {
    std::set<int> currSeeds;
    for(int j=0; j<ancol; j++)  {
      if(kappaMatrix(i, j) >= kappaThr and i!=j) {
        currSeeds.insert(j);
       }
    }
   if(currSeeds.size() >= initialGroupMembership-1) {
    Rcpp::Rcout << "Candidate seeds from row [" << i << "]" << std::endl;
    for(std::set<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
       Rcpp::Rcout << *it << ",";
    }
    Rcpp::Rcout << std::endl;

     if(currSeeds.size() == 1) {
        currSeeds.insert(i);
        seeds.push_back(currSeeds);
      }  else {
        int overThrKappaCnt = 0;
        for(std::set<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
          for(std::set<int>::iterator jt=it; jt!=currSeeds.end(); ++jt) {
             if(jt != it && kappaMatrix(*it, *jt) >= kappaThr) {
                 overThrKappaCnt++;
             }
          }
        }
        int totalKappa = (currSeeds.size()*(currSeeds.size()-1))/2;
        if (overThrKappaCnt >= totalKappa * multiLinkageThr) {
          currSeeds.insert(i);
          seeds.push_back(currSeeds);
        } else {
          Rcpp::Rcout << "Candidate seeds from row [" << i << "] failed because lack of strong linkage" << std::endl;
          Rcpp::Rcout << "total kappa=" << totalKappa;
          Rcpp::Rcout << "strong linkage=" << overThrKappaCnt;
          Rcpp::Rcout << std::endl;
        }
      }
    }
  }

  Rcpp::Rcout << "#Seeds=" << seeds.size() << std::endl;
  for(int i=0; i<seeds.size(); i++) {
    std::set<int> currSeeds = seeds[i];
    Rcpp::Rcout << "Seeds [" << i << "]" << std::endl;
    for(std::set<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
       Rcpp::Rcout << *it << ",";
    }
    Rcpp::Rcout << std::endl;
  }

  // iteratively updating the seeds
  int lastSeedCount;
  int newSeedCount = -1;
  bool changed;
  while(newSeedCount<0 || newSeedCount != lastSeedCount) {
    lastSeedCount = seeds.size();
    changed = FALSE;
    for(int i=0; i<lastSeedCount-1; i++) {
      for(int j=i+1; j<lastSeedCount; j++) {
         Rcpp::Rcout << "ITERATING: i=" << i << ", j=" << j << std::endl;
         std::set<int> seedi = seeds[i];
         std::set<int> seedj = seeds[j];
         std::set<int> intersect;
         std::set<int> ijunion;
         std::set_intersection(seedi.begin(),seedi.end(),
                               seedj.begin(),seedj.end(),
                               std::inserter(intersect,intersect.begin()));
         std::set_union(seedi.begin(),seedi.end(),
                        seedj.begin(),seedj.end(),
                        std::inserter(ijunion,ijunion.begin()));
         int ninter = intersect.size();
         if(ninter >= multiLinkageThr * seedi.size() || ninter >= multiLinkageThr * seedj.size()) {
            seeds[i] = ijunion;
            seeds.erase(seeds.begin() + j);
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
  
  Rcpp::Rcout << "AFTER FILTERING #Seeds=" << seeds.size() << std::endl;
  for(int i=0; i<seeds.size(); i++) {
    std::set<int> currSeeds = seeds[i];
    Rcpp::Rcout << "Seeds [" << i << "]" << std::endl;
    for(std::set<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
       Rcpp::Rcout << *it << ",";
    }
    Rcpp::Rcout << std::endl;
  }

   Rcpp::List seedsRes;
   return(seedsRes);
}')

synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
                    rep(0, 4), rep(1, 7), rep(0,4),
                    rep(c(rep(0,5), rep(1,10)), 3),
                    rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
rownames(synData) <- sprintf("Gene %s", letters[1:8])
colnames(synData) <- sprintf("t%d", 1:15)
davidClustering(synData, removeRedundant = TRUE, debug=TRUE)
davidClustering(t(synData), removeRedundant = TRUE, debug=FALSE)
synKappaMat <- rowKappa(synData)
synKappaMat.round2 <- round(synKappaMat, 2)
testOut1 <- davidClustering_cpp_R(synKappaMat.round2, kappaThr=0.35)

## internal
clus3Kappa <- rowKappa(synData)[c(1,2,3,5,6,7),c(1,2,3,5,6,7)][lower.tri(rowKappa(synData)[c(1,2,3,5,6,7),c(1,2,3,5,6,7)], diag=FALSE)]
