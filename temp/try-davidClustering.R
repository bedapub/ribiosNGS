library(Rcpp)
library(rbenchmark)
library(testthat)
library(ribiosMath)

cppFunction('
Rcpp::List davidClustering_cpp_R(Rcpp::NumericMatrix kappaMatrix,
                               double kappaThr = 0.35,
                               int initialGroupMembership = 3,
                               double multiLinkageThr=0.5,
                               bool debug=0,
                               int mergeRule=1) {
  int anrow = kappaMatrix.nrow();
  int ancol = kappaMatrix.ncol();

  // TODO: seeds is a vector, which is quite expensive with operations like "erase". To be changed into list
  std::vector< std::vector<int> > seeds;
  for(int i=0; i<anrow; i++) {
    std::vector<int> currSeeds;
    for(int j=0; j<ancol; j++)  {
      if(kappaMatrix(i, j) >= kappaThr and i!=j) {
        currSeeds.push_back(j);
       }
    }
   if(currSeeds.size() >= initialGroupMembership-1) {
    if(debug) {
      Rcpp::Rcout << "Candidate seeds from row [" << i << "]" << std::endl;
    for(std::vector<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
       Rcpp::Rcout << *it << ",";
    }
    Rcpp::Rcout << std::endl;
    }
     if(currSeeds.size() == 1) {
        currSeeds.push_back(i);
        seeds.push_back(currSeeds);
      }  else {
        int overThrKappaCnt = 0;
        for(std::vector<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
          for(std::vector<int>::iterator jt=it+1; jt!=currSeeds.end(); ++jt) {
             if(kappaMatrix(*it, *jt) >= kappaThr) {
                 overThrKappaCnt++;
             }
          }
        }
        int totalKappa = (currSeeds.size()*(currSeeds.size()-1))/2;
        if (overThrKappaCnt >= totalKappa * multiLinkageThr) {
          currSeeds.push_back(i);
          seeds.push_back(currSeeds);
        } else {
          if(debug) {
          Rcpp::Rcout << "Candidate seeds from row [" << i << "] failed because lack of strong linkage" << std::endl;
          Rcpp::Rcout << "total kappa=" << totalKappa;
          Rcpp::Rcout << "strong linkage=" << overThrKappaCnt;
          Rcpp::Rcout << std::endl;
          }
        }
      }
    }
  }

  if(debug) {
  Rcpp::Rcout << "#Seeds=" << seeds.size() << std::endl;
  for(int i=0; i<seeds.size(); i++) {
    std::vector<int> currSeeds = seeds[i];
    Rcpp::Rcout << "Seeds [" << i << "]" << std::endl;
    for(std::vector<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
       Rcpp::Rcout << *it << ",";
    }
    Rcpp::Rcout << std::endl;
  }
  }

  // sort seeds, make R indices, and find unique seeds
  for(std::vector< std::vector<int> >::iterator it=seeds.begin(); it!=seeds.end(); ++it) {
    std::sort(it->begin(), it->end());
    std::transform(it->begin(), it-> end(), 
                   it->begin(), std::bind2nd( std::plus<int>(), 1 ) );
  }
  std::sort( seeds.begin(), seeds.end() );
  seeds.erase( std::unique( seeds.begin(), seeds.end() ), seeds.end() );

  // iteratively updating the seeds
  int lastSeedCount;
  int newSeedCount = -1;
  bool changed;
  bool toMerge;
  while(newSeedCount<0 || newSeedCount != lastSeedCount) {
    lastSeedCount = seeds.size();
    changed = FALSE;
    for(int i=0; i<lastSeedCount-1; i++) {
      for(int j=i+1; j<lastSeedCount; j++) {
         std::vector<int> seedi = seeds[i];
         std::vector<int> seedj = seeds[j];
         std::vector<int> intersect;
         std::vector<int> ijunion;
         std::set_intersection(seedi.begin(),seedi.end(),
                               seedj.begin(),seedj.end(),
                               std::back_inserter(intersect));
         std::set_union(seedi.begin(),seedi.end(),
                        seedj.begin(),seedj.end(),
                        std::back_inserter(ijunion));
         int ninter = intersect.size();
         toMerge = FALSE;
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
  
  if(debug) {
  Rcpp::Rcout << "AFTER FILTERING #Seeds=" << seeds.size() << std::endl;
  for(int i=0; i<seeds.size(); i++) {
    std::vector<int> currSeeds = seeds[i];
    Rcpp::Rcout << "Seeds [" << i << "]" << std::endl;
    for(std::vector<int>::iterator it=currSeeds.begin(); it!=currSeeds.end(); ++it) {
       Rcpp::Rcout << *it << ",";
    }
    Rcpp::Rcout << std::endl;
  }
  }

  return(Rcpp::wrap(seeds));
}')

synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
                    rep(0, 4), rep(1, 7), rep(0,4),
                    rep(c(rep(0,5), rep(1,10)), 3),
                    rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
rownames(synData) <- sprintf("Gene %s", letters[1:8])
colnames(synData) <- sprintf("t%d", 1:15)
testExp1 <- davidClustering(synData, removeRedundant = TRUE, debug=FALSE)
testExp2 <- davidClustering(t(synData), removeRedundant = TRUE, debug=FALSE)
synKappaMat <- rowKappa(synData)
synKappaMat.round2 <- round(synKappaMat, 2)
synKappaMatT.round2 <- round(colKappa(synData),2)
testOut1 <- davidClustering_cpp_R(synKappaMat.round2, kappaThr=0.35, debug=TRUE)
testOut2 <- davidClustering_cpp_R(synKappaMatT.round2, kappaThr=0.35)

expect_identical(testExp1, testOut1)
expect_identical(testExp2, testOut2)

benchmark(dcR=davidClustering(synData, removeRedundant = TRUE, debug=FALSE),
          dcCpp=davidClustering_cpp_R(synKappaMatT.round2, kappaThr=0.35))

## large matrix
largeMat <- matrix(rbinom(5000, 1, 0.25), nrow=1000)
largematKappa <- round(rowKappa(largeMat),2)
system.time(rres <- davidClustering(largeMat))
system.time(cppres <- davidClustering_cpp_R(largematKappa, debug=FALSE, kappaThr=0.35))

## note that now the seeds from R and from C++ have different orders
sortedSeeds <- function(x) x[order(sapply(x, paste, collapse=""))]
expect_identical(sortedSeeds(rres), sortedSeeds(cppres))

## currently the Cpp implementation is 8 times faster than the R implementation
benchmark(dcR=davidClustering(largeMat),
          dcCpp=davidClustering_cpp_R(largematKappa))
          
## how different merging methods affect the results?

## internal
clus3Kappa <- rowKappa(synData)[c(1,2,3,5,6,7),c(1,2,3,5,6,7)][lower.tri(rowKappa(synData)[c(1,2,3,5,6,7),c(1,2,3,5,6,7)], diag=FALSE)]
jaccardIndex <- function(x, y=NULL) {
  if(is.null(y))
    y <- x
  sapply(seq(along=x), function(i) sapply(seq(along=y), function(j) length(intersect(x[i],y[j]))/length(union(x[i], y[j]))))
}
