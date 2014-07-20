#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double cosSimilarity(NumericVector a, NumericVector b) {
  double sA, sB, sI;

  sA = sum(a*a);
  sB = sum(b*b);
  sI = sum(a*b);
  return (sI/sqrt(sA * sB));
}

// [[Rcpp::export]]
NumericMatrix matCosSimilarity(NumericMatrix input_mat) {
  int n_row = input_mat.nrow();
  
  NumericMatrix new_mat(n_row, n_row);

  for (int i = 0; i < n_row; ++i) {
    for (int j = i + 1; j < n_row; ++j) {
        new_mat(i, j) = cosSimilarity(input_mat(i, _), input_mat(j, _));
    }    
  }
  
  return new_mat;
}

//// [[Rcpp::export]]
//NumericMatrix dfCosSimilarity(NumericMatrix input_mat) {
//  int n_row = input_mat.nrow();
//  
//  NumericMatrix new_mat(n_row, n_row);
//
//  for (int i = 0; i < n_row; ++i) {
//    for (int j = i + 1; j < n_row; ++j) {
//        new_mat(i, j) = cosSimilarity(input_mat(i, _), input_mat(j, _));
//    }    
//  }
//  
//  return new_mat;
//}
