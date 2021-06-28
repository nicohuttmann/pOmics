#' Returns named vector and substitutes numeric values by quantile groups
#'
#' @param x named numeric vector
#' @param n number of quantiles
#'
#' @return
#' @export
#'
#' 
distribute_quantiles <- function(x, n = 4) {
  
  x <- x[!is.na(x)]
  
  x <- x[x > 0]
  
  y <- x
  
  
  quantiles <- quantile(x, prob = seq(0, 1, 1/n), na.rm = TRUE)
  
  
  for (i in 1:n) {
    
    y[x <= quantiles[n + 2 - i]] <- as.character(i)
    
  }
  
  
  return(y)
  
}
