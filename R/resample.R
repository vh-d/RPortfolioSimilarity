#' generate n x k random matrix, generic version
randMat <- function(n, k, type = runif, ...){
  return(
    matrix(
      do.call(type, args = list(n*k, ...)), 
      nrow = n, ncol = k)
    )
}
