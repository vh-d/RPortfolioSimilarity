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
#' @param simM an optional (n x n) similarity matrix
#' @param weights a (n x n) matrix of weights for weghting matrix to an average
#' @param rw (1 x k) vector of weights on sectors
#' @param arc logical; should the cosine similarities be transformed to radians (by arccos) first?
#' @details This is an R version of the function. Using \code{arc=TRUE} returns (weighted) mean of 1-(2*arccos(cos(similarity))/pi)
#' @return a numeric value of (weighted) mean similarity.
#' @seealso \code{\link{aggSimilarity_time}}
aggSimilarity <- function(
  porftolios, 
  simM    = NULL, 
  weights = NULL, 
  rw      = NULL, 
  arc     = FALSE)
{
  
  if (is.null(simM)) {
    if (is.null(rw)) {
      tmpm <- mCosSimilarity(porftolios)
    } else {
      tmpm <- wtMCosSimilarity(porftolios, weights = rw)
    }
  } else {
    tmpm <- simM
  }
  
  if (arc) {
    tmpm[tmpm > 1 & tmpm < 1 + 2*.Machine$double.eps] <- 1
    tmpm <- 1 - arcCosDist(tmpm)
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

#' arccos distance transformed to [0;1] scale 
#' @param x cosine similarities of positive vectors (as a vector or matrix)
#' @return angular distance
arcCosDist <- function(x) {
  return(2*acos(x)/pi)
}

#' arccos distance transformed to [0;1] scale 
#' @param x cosine similarities of positive vectors (as a vector or matrix)
#' @return angular distance
arcCosSimilarity <- function(input_mat) {
  return(1-arcCosDist(mCosSimilarity(input_mat)))
}

