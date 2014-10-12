#' Mean of the upper triangle of given numerical matrix
#'
#' \code{upperTMean} computes (weighted) mean on the upper triangle of given numerical matrix.
#' @param inmat a (n x k) numerical matrix of portfolios where rows represent n subjects and columns their portfolios
upperTMean <- function(inmat, weights = NULL){
  if (is.null(weights)) {
    return(mean(inmat[upper.tri(inmat)]))
  } else {
    return(weighted.mean(inmat[upper.tri(inmat)], weights[upper.tri(weights)]))
  }
}

#' Aggregate similarity of banking porfolios.
#'
#' \code{aggSimilarity} computes aggregate similarity of banks' portfolios.
#' @param portfolios a (n x k) matrix of portfolios where rows represent n subjects and columns their portfolios
#' @param weights a (n x n) matrix of weights for weghting matrix to an average
#' @param rw (1 x k) vector of weights on sectors
#' @details This is an R version of the function.
#' @return a numeric value of (weighted) mean similarity.
#' @seealso \code{\link{aggSimilarity_time}}
aggSimilarity <- function(porftolios, weights = NULL, rw = NULL){
    if (is.null(rw)) {
      tmpm <- mCosSimilarity(porftolios)
    } else {
      tmpm <- wtMCosSimilarity(porftolios, weights = rw)
    }
    return(upperTMean(tmpm, weights))
}

#' Aggregate similarities of banking porfolios on a list
#'
#' \code{aggSimilarity_time} computes aggregate similarities for a given list of banks' portfolios 
#' @param portfolios_list a list of portfolio matrices where rows represent n subjects and columns their portfolios
#' @param ... other archments past to \code{\link{aggSimilarity}}
#' @details This is an R version of the function.
#' @return a numeric vector.
#' @seealso \code{\link{aggSimilarity}}
aggSimilarity_time <- function(portfolios_list, ...) {
  return(sapply(portfolios_list, aggSimilarity, ...))
}