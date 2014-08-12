#' Aggregate similarity of banking porfolios.
#'
#' \code{aggSimilarity} computes aggregate similarity of banks' portfolios.
#' @param portfolios a (n x k) matrix of portfolios where rows represent n subjects and columns their portfolios
#' @param weights a (n x n) matrix of weights
#' @details This is an R version of the function.
#' @return a numeric value of (weighted) mean similarity.
#' @seealso \code{\link{aggSimilarity_time}}
aggSimilarity <- function(porftolios, weights = NULL){
  if (is.null(weights)) {
    tmpm <- matCosSimilarity(porftolios)
    return(mean(tmpm[upper.tri(tmpm)]))
  } else {
    if (dim(weights) == dim(porftolios)[c(1, 1)]) {
      tmpm <-matCosSimilarity(porftolios)
      return(weighted.mean(tmpm[upper.tri(tmpm)], weights[upper.tri(weights)]))
    } else {
      stop("Weights and portfolios matrices dimensions differ.")
    }
  }
}

#' Aggregate similarities of banking porfolios on a list
#'
#' \code{aggSimilarity_time} computes aggregate similarities for a given list of banks' portfolios 
#' @param portfolios_list a list of portfolio matrices where rows represent n subjects and columns their portfolios
#' @details This is an R version of the function.
#' @return a numeric vector.
#' @seealso \code{\link{aggSimilarity}}
aggSimilarity_time <- function(portfolios_list) {
  return(sapply(portfolios_list, aggSimilarity))
}