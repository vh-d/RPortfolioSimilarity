#' @rdname allPairs
#' @return \code{allPairs} returns (n*(n-1)/2) x 2 matrix all combinations (pairs) of values from vector \code{x} 
#' @export
allPairs <- function(x, unique = TRUE) {
  stopifnot(is.vector(x))
  
  # for scalar value use Rcpp version directly
  if (length(x) == 1 && x == as.integer(x)) return(allPairsRcpp(x))
  
  # drop duplicates?
  if (unique) x <- unique(x)
  
  matrix(x[allPairsRcpp(length(x))], ncol = 2)
}
