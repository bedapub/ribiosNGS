#include "ribios_gsea.h"

RcppExport SEXP list2mat(SEXP data) {
  using namespace std;

  Rcpp::List input(data);
  int n_input=input.size();
  std::list<string> els;
  std::vector<int> outvec;
  std::list<string>::iterator ret;

  for(int i=0;i<n_input;++i) {
    std::vector<string> u = Rcpp::as<std::vector<string> >(input[i]);
    for(std::vector<string>::iterator it=u.begin(); it!=u.end(); ++it) {
      ret=find(els.begin(), els.end(), *it);
      if(ret==els.end()) { // new
        els.push_back(*it);
        for(int j=0; j<n_input; ++j) {
          int push = j==i ? 1 : 0;
          outvec.push_back(push);
        }
      } else {
        outvec[std::distance(els.begin(), ret)*n_input + i]++;
      }
    }

  }
  Rcpp::NumericMatrix M(n_input, els.size(), outvec.begin() );

  std::vector<string> rownames(els.size());
  std::copy(els.begin(), els.end(), rownames.begin());
  Rcpp::List dimms = Rcpp::List::create(input.names(), rownames);
  M.attr("dimnames")=dimms;
  return M;
}
