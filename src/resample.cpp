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

//' Generate random variation of a matrix keeping rows and cols sums. Deprecated, use restRandMat().
//'
//'@param original_mat the original matrix that will be replicated
//'@param max_iter maximum number of iterations when coverging to the rows and cols sums
//'@return numeric matrix with the same dimensions and (almost) rows' and columns' sums as the input matrix
// [[Rcpp::export]]
NumericMatrix restRandMatUnif(NumericMatrix original_mat, long int max_iter) {

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
    new_mat(i, _) = runif(ncol); // fill the i-th row, draw from uniform distribution
    
    rowsum = sum(new_mat(i, _));
    sum_ratio = rows_sums_orig[i] / rowsum; // calculate the ratio of row sums from the original and the new matrix
    new_mat(i, _) = new_mat(i, _) * sum_ratio; // scale the new row to have the same sum as the original 
  }
  
  // iterate until maximum number of trials is reached or ...?
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

//' Generate random variation of a matrix keeping rows and cols sums. Deprecated, use restRandMat().
//'
//'@param original_mat the original matrix that will be replicated
//'@param max_iter maximum number of iterations when coverging to the rows and cols sums
//'@param meanl parameter of the log-normal distribution
//'@param sdl parameter of the log-normal distribution
//'@return numeric matrix with the same dimensions and (almost) rows' and columns' sums as the input matrix
// [[Rcpp::export]]
NumericMatrix restRandMatLNorm(NumericMatrix original_mat, long int max_iter, double meanl = 0, double sdl = 1) {

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
    new_mat(i, _) = rlnorm(ncol,  meanl, sdl); // fill the i-th row, draw from log-normal distribution
    
    rowsum = sum(new_mat(i, _));
    sum_ratio = rows_sums_orig[i] / rowsum; // calculate the ratio of row sums from the original and the new matrix
    new_mat(i, _) = new_mat(i, _) * sum_ratio; // scale the new row to have the same sum as the original 
  }
  
  // iterate until maximum number of trials is reached or ...?
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

//' Generate random variation of a matrix keeping rows and cols sums. Deprecated, use restRandMat().
//'
//'@param original_mat the original matrix that will be replicated
//'@param max_iter maximum number of iterations when coverging to the rows and cols sums
//'@param shape parameter of the gamma distribution
//'@param rate parameter of the gamma distribution
//'@return numeric matrix with the same dimensions and (almost) rows' and columns' sums as the input matrix
// [[Rcpp::export]]
NumericMatrix restRandMatGamma(NumericMatrix original_mat, long int max_iter, double shape, double rate = 1.0) {

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
    new_mat(i, _) = rgamma(ncol,  shape, rate); // fill the i-th row, draw from gamma distribution
    
    rowsum = sum(new_mat(i, _));
    sum_ratio = rows_sums_orig[i] / rowsum; // calculate the ratio of row sums from the original and the new matrix
    new_mat(i, _) = new_mat(i, _) * sum_ratio; // scale the new row to have the same sum as the original 
  }
  
  // iterate until maximum number of trials is reached or ...?
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



//' Generate random variation of a matrix keeping rows and cols sums
//'
//'@param original_mat the original matrix that will be replicated
//'@param max_iter maximum number of iterations when coverging to the rows and cols sums
//'@return numeric matrix with the same dimensions and (almost) rows' and columns' sums as the input matrix
// [[Rcpp::export]]
NumericMatrix restRandMat(NumericMatrix original_mat, long int max_iter, int type, double par1 = 1.0, double par2 = 1.0) {

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
    
    // fill the i-th row, draw from uniform distribution
    switch (type)
    {
      
    case 1: 
      new_mat(i, _) = runif(ncol); 
      break;
    case 2:
      new_mat(i, _) = rlnorm(ncol, par1, par2);
      break;
    case 3:
      new_mat(i, _) = rgamma(ncol, par1, par2);
      break;
    default:
      new_mat(i, _) = runif(ncol);
    }
    
    rowsum = sum(new_mat(i, _));
    sum_ratio = rows_sums_orig[i] / rowsum; // calculate the ratio of row sums from the original and the new matrix
    new_mat(i, _) = new_mat(i, _) * sum_ratio; // scale the new row to have the same sum as the original 
  }
  
  // iterate until maximum number of trials is reached or ...?
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
