#include <Rcpp.h>
using namespace Rcpp;

//' Compute cosine similarity of two numeric vectors
//'
//'@param a numeric vector
//'@param b numeric vector
//'@return Numeric value of cosine similarity
// [[Rcpp::export]]
double vCosSimilarity(NumericVector a, NumericVector b) {
  double sA, sB, sI;

  sA = sum(a*a);
  sB = sum(b*b);
  sI = sum(a*b);
  return (sI/sqrt(sA * sB));
}

//' Compute weighted cosine similarity of two numeric vectors
//'
//'@param a numeric vector
//'@param b numeric vector
//'@param w numeric vector of weights
//'@return Numeric value of cosine similarity
// [[Rcpp::export]]
double wtVCosSimilarity(NumericVector a, NumericVector b, NumericVector w) {
  double sA, sB, sI;

  sA = sum(w*a*a);
  sB = sum(w*b*b);
  sI = sum(w*a*b);
  return (sI/sqrt(sA * sB));
}


//' Compute weighted cosine similarity of two numeric vectors
//'
//'@param a numeric vector
//'@param b numeric vector
//'@param w numeric vector of weights
//'@return Numeric value of cosine similarity
// [[Rcpp::export]]
double sftVCosSimilarity(NumericVector a, NumericVector b, NumericMatrix weights) {
  
  int la = a.length();
  int lb = b.length();
  int n_row = weights.nrow();
  int n_col = weights.ncol();
  
  if ((la != lb) | (n_row != n_col) | (la != n_row)) {
    // Rcpp::Rcerr << "Key not found: "<< key << std::endl;
    Rcpp::stop("Dimension mismatch!");
  }
    
  double sA, sB, sI;
  
  sI = sB = sA = 0.0;
  
  for (int i = 0; i < n_row; ++i) {
    for (int j = 0; j < n_col; ++j) {
      sA += a[i] * a[j] * weights(i, j);
      sB += b[i] * b[j] * weights(i, j);
      sI += a[i] * b[j] * weights(i, j);
    } 
  }
  
  return (sI/sqrt(sA * sB));
}


//' Compute cosine similarity for every pair of rows from given matrix
//'
//'@param input_mat numeric input matrix
//'@return Upper triangle matrix where \{i, j\} element is the cosine similarity of i-th and j-th row of the original matrix.
// [[Rcpp::export]]
NumericMatrix mCosSimilarity(NumericMatrix input_mat) {
  int n_row = input_mat.nrow();
  
  NumericMatrix new_mat(n_row, n_row);

  for (int i = 0; i < n_row; ++i) {
    for (int j = i + 1; j < n_row; ++j) {
        new_mat(i, j) = vCosSimilarity(input_mat(i, _), input_mat(j, _));
    }    
  }
  
  return new_mat;
}

//' Compute weighted cosine similarities for each pair of rows from given matrix and given weights
//'
//'@param input_mat numeric input matrix
//'@param weights numeric vector of weights
//'@return Upper triangle matrix where \{i, j\} element is the cosine similarity of i-th and j-th row of the original matrix.
// [[Rcpp::export]]
NumericMatrix wtMCosSimilarity(NumericMatrix input_mat, NumericVector weights) {
  int n_row = input_mat.nrow();
  
  NumericMatrix new_mat(n_row, n_row);

  for (int i = 0; i < n_row; ++i) {
    for (int j = i + 1; j < n_row; ++j) {
        new_mat(i, j) = wtVCosSimilarity(input_mat(i, _), input_mat(j, _), weights);
    }    
  }
  
  return new_mat;
}

//' Compute soft cosine similarities for each pair of rows from given matrix and given weights
//'
//'@param input_mat numeric input matrix
//'@param weights numeric vector of weights
//'@return Upper triangle matrix where \{i, j\} element is the cosine similarity of i-th and j-th row of the original matrix.
// [[Rcpp::export]]
NumericMatrix sftMCosSimilarity(NumericMatrix input_mat, NumericMatrix weights) {
  int n_row = input_mat.nrow();
  
  NumericMatrix new_mat(n_row, n_row);

  for (int i = 0; i < n_row; ++i) {
    for (int j = i + 1; j < n_row; ++j) {
        new_mat(i, j) = sftVCosSimilarity(input_mat(i, _), input_mat(j, _), weights);
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
