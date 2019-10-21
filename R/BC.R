#' Bimodality criterion
#'
#' @param a numeric vector
#'
#' @return a numeric value
#' @export
#'
#' @examples
#' BC(rnorm(1000))
#'
BC <- function(Sequence){

  n <- length(Sequence)
  result <- ( e1071::skewness( Sequence )^2 + 1 ) / ( e1071::kurtosis( Sequence ) + 3 * ( ( (n-1)^2) / ( (n-2) * (n-3) ) ) )
  return( result )
}
