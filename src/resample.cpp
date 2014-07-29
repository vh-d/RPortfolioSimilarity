#include <Rcpp.h>
using namespace Rcpp;

NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(); 
  int ncol = x.ncol();
  
  NumericVector out(nrow);
  
  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}

NumericVector colSumsC(NumericMatrix x) {
  int nrow = x.nrow(); 
  int ncol = x.ncol();
  
  NumericVector out(ncol);
  
  for (int i = 0; i < ncol; i++) {
    double total = 0;
    for (int j = 0; j < nrow; j++) {
      total += x(j, i);
    }
    out[i] = total;
  }
  return out;
}

// // [[Rcpp::export]]
//NumericMatrix resample_matrix(NumericMatrix original_mat, long int max_iter) {
//  #define MAX_TRIALS max_iter // limit on interations, throw error if reached without result
//
//  // get dimensions of the original matrix
//  int nrow = original_mat.nrow();
//  int ncol = original_mat.ncol();
//  
//  // get rows and column sums - constraints of the new matrix
//  NumericVector rows_sums_orig = rowSumsC(original_mat);
//  NumericVector col_sums_orig = colSumsC(original_mat);
//  
//  // create the new matrix with the dimensions of the original matrix 
//  NumericMatrix new_mat(nrow, ncol);
//  
//  NumericVector rows_sums_new(nrow);
//  NumericVector resid(nrow);
//  
//  int trial = 0;
//  double colsum = 0;
//  double sum_ratio = 0;
//  
//  // cycle untill maximum number of trials is reached or the new matrix is found
//  do {
//    trial++; // increase number of trials
//    new_mat.fill(0); // reset the new matrix
//    resid.fill(0); // reset the residuum vector
//    
//    // fill the new matrix by random numbers from uniform distribution
//    for (int j = 0; (j < (ncol - 1)) && is_true(all(resid >= 0)); ++j) {
//      new_mat(_, j) = runif(nrow); // fill the j-th column
//
//      colsum = sum(new_mat(_, j));
//      sum_ratio = col_sums_orig[j] / colsum; // calculate the ratio of column sums from the original and the new matrix
//      new_mat(_, j) = new_mat(_, j) * sum_ratio; // scale the new column to have the same sum as the original 
//      
//      // calculate the rows sums and the last column as residual
//      rows_sums_new = rowSumsC(new_mat);
//      resid = rows_sums_orig - rows_sums_new;
//    }
//
//    new_mat(_, ncol-1) = resid; // last column is the vector of residuals
//    
//  } while (is_true(any(resid < 0)) && trial < MAX_TRIALS);
//  
//  if (trial == MAX_TRIALS) {
//    Rcpp::stop("Reached maximum number of trials");  
//  }
//  return new_mat;
//}

// [[Rcpp::export]]
NumericMatrix randMatrix(NumericMatrix original_mat, long int max_iter) {

  // get dimensions of the original matrix
  long int nrow = original_mat.nrow();
  long int ncol = original_mat.ncol();
  
  // get rows and column sums - constraints of the new matrix
  NumericVector rows_sums_orig = rowSumsC(original_mat);
  NumericVector cols_sums_orig = colSumsC(original_mat);
  
  NumericVector rows_sums_new(nrow);
  NumericVector cols_sums_new(ncol);

 // create the new matrix with the dimensions of the original matrix 
  NumericMatrix new_mat(nrow, ncol);

  long int trial = 0;
  double rowsum = 0;
  double sum_ratio = 0;
  
  long int irow;
  long int icol;
  
  // fill the new matrix by random numbers from uniform distribution
  for (int i = 0; i < nrow; ++i) {
    runif(ncol);
    new_mat(i, _) = runif(ncol); // fill the i-th row
    
    rowsum = sum(new_mat(i, _));
    sum_ratio = rows_sums_orig[i] / rowsum; // calculate the ratio of row sums from the original and the new matrix
    new_mat(i, _) = new_mat(i, _) * sum_ratio; // scale the new row to have the same sum as the original 
  }
  
//   cycle untill maximum number of trials is reached or the new matrix is found
  do {
    trial++; // increase number of trials
    
    cols_sums_new = colSumsC(new_mat);
    icol = which_max(abs(cols_sums_new - cols_sums_orig));
    new_mat(_, icol) = new_mat(_, icol) * (cols_sums_orig[icol] / cols_sums_new[icol]);
    
    rows_sums_new = rowSumsC(new_mat);
    irow = which_max(abs(rows_sums_new - rows_sums_orig));
    new_mat(irow, _) = new_mat(irow, _) * (rows_sums_orig[irow] / rows_sums_new[irow]);
    
  } while (trial < max_iter);
  
  return(new_mat);
}
