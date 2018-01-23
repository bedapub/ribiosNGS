library(Rcpp)

cppFunction('
Rcpp::List davidClustering_cpp_R(Rcpp::NumericMatrix kappaMatrix,
                               double kappaThr = 0.35,
                               int initialGroupMembership = 3) {
  int anrow = kappaMatrix.nrow();
  int ancol = kappaMatrix.ncol();
  
  Rcpp::List seeds;
  for(int i=0; i<anrow; i++) {
    Rcpp::IntegerVector currSeeds(1, i);
    for(int j=0; j<ancol; j++)  {
      if(kappaMatrix(i, j) >= kappaThr and i!=j) {
        currSeeds.push_back(j);
        Rcpp::Rcout << "i=" << i << ", j=" << j << std::endl;
      }
    }
    seeds.push_back(currSeeds);
  }
  return(seeds);
}')

testMat <- cbind(c(1,1,0,0,1,0), c(1,1,0,1,1,0), c(1,0,1,1,0,1))
testKappaMat <- rowKappa(testMat)
davidClustering_cpp_R(testKappaMat)
