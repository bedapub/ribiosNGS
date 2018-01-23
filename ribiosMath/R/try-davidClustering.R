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
  
  Rcpp::List seeds;
  for(int i=0; i<anrow; i++) {
    Rcpp::IntegerVector currSeeds(1, i);
    for(int j=0; j<ancol; j++)  {
      if(kappaMatrix(i, j) >= kappaThr and i!=j) {
        currSeeds.push_back(j);
       }
    }
   if(currSeeds.size() >= initialGroupMembership) {
     if(currSeeds.size()<2) {
        seeds.push_back(currSeeds);
      }  else {
        Rcpp::NumericVector subKappas;
        for(int i=1; i<currSeeds.size(); i++) {
          for(int j=i+1; j<currSeeds.size(); j++) {
             subKappas.push_back(kappaMatrix(currSeeds[i],
                                             currSeeds[j]));
          }
        }
        double sum=0;
        for(int i=0; i<subKappas.size(); i++) {
           sum += subKappas[i];
        }
        if(sum/subKappas.size()>=multiLinkageThr) {
           seeds.push_back(currSeeds);
        }
      }
   }
  }
  
  Rcpp::Rcout << "#Seeds i:" << seeds.size() << std::endl;
  for(int i=0; i<seeds.size(); i++) {
    NumericVector currSeeds = seeds[i];
    Rcpp::Rcout << currSeeds << std::endl;
  }

  // iterative updating the seeds
  int lastSeedCount = seeds.size();
  int newSeedCount = -1;
  bool changed = 0;
  //while(newSeedCount<0 || newSeedCount != lastSeedCount) {
    lastSeedCount = seeds.size();
    for(int i=0; i<lastSeedCount-1; i++) {
      for(int j=i+1; j<lastSeedCount; j++) {
         Rcpp::IntegerVector seedsi = seeds[i];
         Rcpp::IntegerVector seedsj = seeds[j];
         // Rcpp::IntegerVector seedsij = seedsi.insert(seedsi.end(), seedsj.begin(), seedsj.end());
         // Rcpp::IntegerVector seedsijUniq = unique(seedsij);
         // check linkage
        double linkage = 0.2;
       // if(linkage >= multiLinkageThr) {
       //    seeds[i] = seedsijUniq;
       //    seeds.erase(j);
       //   changed = 1;
       //    break;
       // }
      }
      if(changed) {
          break;
      }
    }
  // }
  return(seeds);
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
testOut1 <- davidClustering_cpp_R(synKappaMat.round2)
