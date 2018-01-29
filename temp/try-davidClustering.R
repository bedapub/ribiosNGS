library(Rcpp)
library(rbenchmark)
library(testthat)
library(ribiosMath)
library(ribiosGSEA)
library(ribiosIO)
library(ribiosUtils)


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

## TODO: how different merging methods affect the results?

## list instead of vector
cppFunction('
Rcpp::List davidClustering_cpplist_R(Rcpp::NumericMatrix kappaMatrix,
                               double kappaThr = 0.35,
                               int initialGroupMembership = 3,
                               double multiLinkageThr=0.5,
                               bool debug=0,
                               int mergeRule=1) {
  int anrow = kappaMatrix.nrow();
  int ancol = kappaMatrix.ncol();

  // TODO: seeds is a vector, which is quite expensive with operations like "erase". To be changed into list
  std::list< std::vector<int> > seeds;
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

  // sort seeds, make R indices, and find unique seeds
  for(std::list< std::vector<int> >::iterator it=seeds.begin(); it!=seeds.end(); ++it) {
    std::sort(it->begin(), it->end());
    std::transform(it->begin(), it-> end(), 
                   it->begin(), std::bind2nd( std::plus<int>(), 1 ) );
  }
  seeds.sort();
  seeds.erase( std::unique( seeds.begin(), seeds.end() ), seeds.end() );

  // iteratively updating the seeds
  int lastSeedCount;
  int newSeedCount = -1;
  bool changed;
  bool toMerge;
  while(newSeedCount<0 || newSeedCount != lastSeedCount) {
    lastSeedCount = seeds.size();
    changed = FALSE;
    for(std::list< std::vector<int> >::iterator it=seeds.begin(); it!=seeds.end(); ++it) {
     for(std::list< std::vector<int> >::iterator jt=it; jt!=seeds.end(); ++jt) {
      if(jt==it) {
        jt++;
        if(jt==seeds.end()) 
          break;
      }
         std::vector<int> seedi = *it;
         std::vector<int> seedj = *jt;
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
  
  return(Rcpp::wrap(seeds));
}')

## 
davidClusteringR <- function(matKappa.round2, 
                            kappaThr = 0.35,
                            initialGroupMembership=3,
                            multiLinkageThr=0.5,
                            ## the following parameters are used for debugging
                            removeRedundant=TRUE,
                            debug=FALSE) {
  matBin <- matKappa.round2 >= kappaThr
  ## note that order in seeds is important: the first element is the initial seed
  seeds <- sapply(1:nrow(matBin), function(i) {
    x <- matBin[i,]
    members <- which(x)
    c(i, setdiff(members, i))
  })
  if(removeRedundant) {
    ordSeeds <- lapply(seeds, sort)
    dupSeeds <- duplicated(ordSeeds)
    seeds <- seeds[!dupSeeds]
  }
  seeds <- seeds[sapply(seeds, length)>=initialGroupMembership]
  if(length(seeds)==0)
    return(list())
  
  isMultiLinkage <- sapply(seeds, function(x) {
    nonInitialSeed <- x[-1]
    subKappa <- matKappa.round2[nonInitialSeed, nonInitialSeed]
    pwKappa <- subKappa[lower.tri(subKappa, diag=FALSE)]
    isLinked <- mean(pwKappa >= kappaThr)>=multiLinkageThr
    return(isLinked)
  })
  seeds <- seeds[isMultiLinkage]
  
  ## iteratively merge seeds until no two seeds share majority (multiLinkageThr) of members
  if(length(seeds)>1) {
    lastSeedCount <- length(seeds)
    newSeedCount <- NA
    while(is.na(newSeedCount) || lastSeedCount!=newSeedCount) {
      if(debug) {
        print(seeds)
      }
      lastSeedCount <- length(seeds)
      changed <- FALSE
      for(i in seq(1, lastSeedCount-1)) {
        for(j in seq(i+1, lastSeedCount)) {
          if(j>length(seeds))
            break
          seedsi <- seeds[[i]]
          seedsj <- seeds[[j]]
          commonLen <- length(intersect(seedsi, seedsj))
          total <- union(seedsi, seedsj)
          totalLen <- length(total)
          linkage <- commonLen/totalLen
          if(linkage >= multiLinkageThr) {
            seeds[[i]] <- total
            seeds <- seeds[-j]
            changed <- TRUE
            break
          }
        }
        if(changed) {
          break
        }
      }
      newSeedCount <- length(seeds)
      if(debug) {
        cat(sprintf("lastSeedCount=%d, newSeedCount=%d, i=%d, j=%d\n", lastSeedCount, newSeedCount, i, j))
      }
    }
  }
  ## sort the seeds
  seeds <- lapply(seeds, sort, decreasing=FALSE)
  return(seeds)
}

## benchmark

synData <- matrix(c(rep(c(rep(1, 10), rep(0, 5)), 3),
                    rep(0, 4), rep(1, 7), rep(0,4),
                    rep(c(rep(0,5), rep(1,10)), 3),
                    rep(c(rep(0,3), 1), 4)[-16]), ncol=15, byrow=TRUE)
rownames(synData) <- sprintf("Gene %s", letters[1:8])
colnames(synData) <- sprintf("t%d", 1:15)
synKappaMat <- rowKappa(synData)
synKappaMat.round2 <- round(synKappaMat, 2)
synKappaMatTp <- colKappa(synData)
synKappaMatTp.round2 <- round(synKappaMatTp, 2)
testExp1 <- davidClusteringR(synKappaMat.round2, removeRedundant = TRUE, debug=FALSE)
testExp2 <- davidClusteringR(synKappaMatTp.round2, removeRedundant = TRUE, debug=FALSE)

synKappaMatT.round2 <- round(colKappa(synData),2)
testOut1 <- davidClustering_cpp_R(synKappaMat.round2, kappaThr=0.35, debug=TRUE)
testOut2 <- davidClustering_cpp_R(synKappaMatT.round2, kappaThr=0.35)

testlistOut1 <- davidClustering_cpplist_R(synKappaMat.round2, kappaThr=0.35, debug=TRUE)
testlistOut2 <- davidClustering_cpplist_R(synKappaMatT.round2, kappaThr=0.35)

expect_identical(testExp1, testOut1)
expect_identical(testExp2, testOut2)
expect_identical(testExp1, testlistOut1)
expect_identical(testExp2, testlistOut2)

benchmark(dcR=davidClustering(synData, removeRedundant = TRUE, debug=FALSE),
          dcCppVec=davidClustering_cpp_R(synKappaMatT.round2, kappaThr=0.35),
          dcCppList=davidClustering_cpplist_R(synKappaMatT.round2, kappaThr=0.35))

## large matrix
set.seed(1887)
largeMat <- matrix(rbinom(10000, 1, 0.25), nrow=1000)
largematKappa <- round(rowKappa(largeMat),2)
system.time(rres <- davidClusteringR(largematKappa))
system.time(cppres <- davidClustering_cpp_R(largematKappa, debug=FALSE, kappaThr=0.35))
system.time(cpplistres <- davidClustering_cpplist_R(largematKappa, debug=FALSE, kappaThr=0.35))

## note that now the seeds from R and from C++ have different orders
sortedSeeds <- function(x) x[order(sapply(x, paste, collapse=""))]
expect_identical(sortedSeeds(rres), sortedSeeds(cppres))
expect_identical(sortedSeeds(cppres), sortedSeeds(cpplistres))

## currently the Cpp implementation is 8 times faster than the R implementation
## the performance of std::list< std::vector<int> > and std::vector< std::vector<int> > is comparable, though vector::erase should be much more expensive than list::erase
## in certain cases list is even slower
benchmark(dcR=davidClustering(largeMat),
          dcCppVec=davidClustering_cpp_R(largematKappa),
          dcCppList=davidClustering_cpplist_R(largematKappa))
          
## Try figuring out which merging method makes more sense
davidFile <- "david-clustering-example.txt"
davidRes <- read_david(davidFile)
probeAnno <- readMatrix("probe-annotation.txt", as.matrix=FALSE, row.names=FALSE)
david2cluster <- function(res) {
  split(1:nrow(res), res$cluster)
}
david2matrix <- function(res, annotation) {
  ## clusters <- res$cluster
  terms <- with(res, paste(Category, Term, sep="|"))
  probes <- lapply(as.character(res$Genes), function(x) sapply(strsplit(x, ",")[[1]], ribiosUtils::trim))
  genes <- lapply(probes, function(p) {
    genes <- matchColumn(tolower(p), annotation, "AFFYMETRIX_3PRIME_IVT_ID")$Name
    genes <- setdiff(genes[!is.na(genes)], "")
    return(genes)
  })
  uniqGenes <- ribiosUtils::munion(genes)
  termByGene <- t(sapply(genes, function(x) uniqGenes %in% x))
  rownames(termByGene) <- terms
  colnames(termByGene) <- uniqGenes
  stopifnot(all(rowSums(termByGene) == res$Count)) ## consistent count of genes
  return(termByGene)
}
dim(davidMatrix <- david2matrix(davidRes, probeAnno))
davidKappa <- rowKappa(davidMatrix)
davidKappa2 <- rowKappa(davidMatrix+1-1)
expect_identical(davidKappa, davidKappa2)
davidKappa.round2 <- round(rowKappa(davidMatrix),2)
ulen(davidRes$cluster)
length(davidOrigClus <- david2cluster(davidRes))
length(davidClus1 <- davidClustering_kappa(davidKappa, kappaThr = 0.5, mergeRule=1))
length(davidClus2 <- davidClustering_kappa(davidKappa, kappaThr = 0.5, mergeRule=2))
length(davidClus3 <- davidClustering_kappa(davidKappa, kappaThr = 0.5, mergeRule=3))
## length(davidClus4 <- davidClustering_kappa(davidKappa, kappaThr = 0.5, mergeRule=4))
## length(davidClus5 <- davidClustering_kappa(davidKappa, kappaThr = 0.5, mergeRule=5))

writeCluster <- function(davidRes, clusterList, file) {
  terms <- sapply(clusterList, function(x) as.character(davidRes$Term[x]))
  names <- sprintf("Cluster %d", seq(along=clusterList))
  writeStrList(terms, file, names=names)
}
davidList <- with(davidRes, split(as.character(Term), cluster))

writeCluster(davidRes, davidClus1, "davidClus1.txt")
writeCluster(davidRes, davidClus2, "davidClus2.txt")
writeCluster(davidRes, davidClus3, "davidClus3.txt")
writeStrList(davidList, "davidOrig.txt", names=names(davidList))

midentical(davidClus1,davidClus2, davidClus3, davidClus4)
ribiosPlot::biosHeatmap(jaccardIndex(davidClus1, davidOrigClus), ylab="Original DAVID", Colv=FALSE, Rowv=FALSE)
ribiosPlot::biosHeatmap(jaccardIndex(davidClus2, davidOrigClus), ylab="Original DAVID", Colv=FALSE, Rowv=FALSE)
ribiosPlot::biosHeatmap(jaccardIndex(davidClus3, davidOrigClus), ylab="Original DAVID", Colv=FALSE, Rowv=FALSE)
ribiosPlot::biosHeatmap(jaccardIndex(davidClus4, davidOrigClus), ylab="Original DAVID", Colv=FALSE, Rowv=FALSE)

## internal
clus3Kappa <- rowKappa(synData)[c(1,2,3,5,6,7),c(1,2,3,5,6,7)][lower.tri(rowKappa(synData)[c(1,2,3,5,6,7),c(1,2,3,5,6,7)], diag=FALSE)]
jaccardIndex <- function(x, y=NULL) {
  if(is.null(y)) {
    y <- x
  }
  res <- sapply(seq(along=x), function(i) {
    sapply(seq(along=y), function(j) {
      currX <- x[[i]]
      currY <- y[[j]]
      res <- length(intersect(currX, currY))/length(union(currX, currY))
      return(res)
    })
  })
  return(res)
}
