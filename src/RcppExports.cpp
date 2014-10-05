// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// randMatrixUnif
NumericMatrix randMatrixUnif(NumericMatrix original_mat, long int max_iter);
RcppExport SEXP RPortfolioSimilarity_randMatrixUnif(SEXP original_matSEXP, SEXP max_iterSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericMatrix >::type original_mat(original_matSEXP );
        Rcpp::traits::input_parameter< long int >::type max_iter(max_iterSEXP );
        NumericMatrix __result = randMatrixUnif(original_mat, max_iter);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// randMatrixLNorm
NumericMatrix randMatrixLNorm(NumericMatrix original_mat, long int max_iter);
RcppExport SEXP RPortfolioSimilarity_randMatrixLNorm(SEXP original_matSEXP, SEXP max_iterSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericMatrix >::type original_mat(original_matSEXP );
        Rcpp::traits::input_parameter< long int >::type max_iter(max_iterSEXP );
        NumericMatrix __result = randMatrixLNorm(original_mat, max_iter);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// vCosSimilarity
double vCosSimilarity(NumericVector a, NumericVector b);
RcppExport SEXP RPortfolioSimilarity_vCosSimilarity(SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP );
        double __result = vCosSimilarity(a, b);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// wtVCosSimilarity
double wtVCosSimilarity(NumericVector a, NumericVector b, NumericVector w);
RcppExport SEXP RPortfolioSimilarity_wtVCosSimilarity(SEXP aSEXP, SEXP bSEXP, SEXP wSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericVector >::type a(aSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type b(bSEXP );
        Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP );
        double __result = wtVCosSimilarity(a, b, w);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mCosSimilarity
NumericMatrix mCosSimilarity(NumericMatrix input_mat);
RcppExport SEXP RPortfolioSimilarity_mCosSimilarity(SEXP input_matSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< NumericMatrix >::type input_mat(input_matSEXP );
        NumericMatrix __result = mCosSimilarity(input_mat);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
