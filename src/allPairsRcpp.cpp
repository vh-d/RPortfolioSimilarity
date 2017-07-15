#include <Rcpp.h>
using namespace Rcpp;

//' Generate all possible pairs from a vector or a sequence of integer numbers
//'
//' @rdname allPairs
//' @param n maximum number of the sequence
//' @return \code{allPairsRcpp} returns (n*(n-1)/2) x 2 integer matrix all unique combinations (pairs) of numbers from a sequence of integers 1..n 
// [[Rcpp::export]]
IntegerMatrix allPairsRcpp(int n) {
  
  int n_row = n*(n-1)/2;
  IntegerMatrix res(n_row, 2);
  
  int k = 0;
  for (int i = 1; i < n; ++i) {
    for (int j = i + 1; j < n+1; ++j) {
      res(k, 0) = i;
      res(k, 1) = j;
      k++;
    }
  }
  
  return res;
}
