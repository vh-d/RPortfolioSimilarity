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

#' Aggregate similarity
#'
#' \code{aggSimilarity} computes aggregate similarity
#' @param portfolios a (n x k) matrix of portfolios where rows represent n subjects and columns their portfolios
#' @param simM an optional (n x n) similarity matrix
#' @param weights a (n x n) matrix of weights for weghting matrix to an average
#' @param rw (1 x k) vector of weights on sectors
#' @param arc logical; should the cosine similarities be transformed to radians (by arccos) first?
#' @details Using \code{arc=TRUE} returns (weighted) mean of 1-(2*arccos(cos(similarity))/pi)
#' @return A numeric scalar value of (weighted) mean of the upper traingle of similarity matrix is returned.
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


#' Aggregate similarity applied to a list
#'
#' \code{aggSimilarity_time} computes aggregate similarities for a given list of portfolios 
#' @param portfolios_list a list of portfolio matrices where rows represent n subjects and columns their portfolios
#' @param ... other archments past to \code{\link{aggSimilarity}}
#' @return A numeric vector of the same lenght as input.
#' @seealso \code{\link{aggSimilarity}}
aggSimilarity_time <- function(portfolios_list, ...) {
  return(sapply(portfolios_list, aggSimilarity, ...))
}

equal <- function(x, value) {
  abs(x - value) < sqrt(.Machine$double.eps)
}

#' arccos distance transformed to [0;1] scale 
#' @param x cosine similarities of positive vectors (as a vector or matrix)
#' @return angular distance
arcCosDist <- function(x) {
  ret <- 2*acos(x)/pi
  ret[equal(x, 1.0)] <- 0 # handle double precision issues
  
  return(ret)
}

#' arccos distance transformed to [0;1] scale 
#' @param x cosine similarities of positive vectors (as a vector or matrix)
#' @return angular distance
arcCosSimilarity <- function(input_mat) {
  return(1-arcCosDist(mCosSimilarity(input_mat)))
}

fromUpperTri <- function(x) {
  x[lower.tri(x)] <- t(x)[lower.tri(t(x))]
  
  return(x)
} 


#' generic cosine similarity function
#' @param x        numeric (vector or matrix)
#' @param y        if x is numeric vecor, y has to be numeric vector of the same length
#' @param weights  numeric; weights for weighted or soft cosine similarity
#' @param complete logical; if FALSE (default) only upper triangle of the similarity matrix is computed
cosine <- 
  function(
    x, 
    y        = NULL, 
    weights  = NULL,
    complete = FALSE
  ) {
    
    if (is.null(y))  {
      if (is.numeric(x) || is.matrix(x)) {
        if (is.null(weights)) {
          out <- wtMCosSimilarity(x,    weights)
        } else {
          if (is.matrix(weights)) {
            out <- sftMCosSimilarity(x, weights)
          } else {
            out <- mCosSimilarity(x) 
          }
        }
      } else stop("x must be a numeric matrix!")
    } else {
      if (is.numeric(x) && 
          is.numeric(y) && 
          is.vector(x)  && 
          is.vector(y)  &&
          length(x) == length(y)) 
      {
        if (is.null(weights)) {
          out <- vCosSimilarity(x, y)
        } else {
          if (is.matrix(weights)) {
            out <- sftVCosSimilarity(x, y, weights)
          } else {
            out <- wtVCosSimilarity(x, y,  weights)
          }
        }
      } else stop("Both x and y must be a numeric vectors of the same size!")
    }
    
    if (complete) {
      out <- fromUpperTri(out)
      diag(out) <- 1
    }
    
    return(out)
  }

