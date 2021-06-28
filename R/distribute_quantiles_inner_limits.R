#' Returns named vector and substitutes numeric values by quantile groups
#'
#' @param x named numeric vector
#' @param n number of quantiles
#'
#' @return
#' @export
#'
#' 
distribute_quantiles_inner_limits <- function(x, n = 4) {
  
  x <- x[!is.na(x)]
  
  x <- x[x > 0]
  
  y <- x
  
  
  quantiles <- quantile(x, prob = seq(0, 1, 1/n), na.rm = TRUE)
  
  
  
  return(quantiles[2:(length(quantiles) - 1)])
  
}
