#' Imputes values based on normal deviation
#'
#' @param data data
#' @param shift shift in standard deviations
#' @param width width in standard deviations
#' @param seed seed
#'
#' @return
#' @export
#'
#'
impute_norm <- function(data, shift = 1.8, width = 0.3, seed = 123) {
  
  # Prepare log2 data frame
  data1 <- data
  data1[data1 == 0] <- NA
  data1 <- log2(data1)
  
  set.seed(seed)
  
  # Impute
  for (i in which(apply(X = data, MARGIN = 2, FUN = function(x) any(x == 0)))) {
    
    data[data[, i] == 0, i] <- round(2 ^ rnorm(n = sum(data[, i] == 0),
                                               mean = mean(data1[, i], na.rm = TRUE) - shift * sd(data1[, i], na.rm = TRUE),
                                               sd = width * sd(data1[, i], na.rm = TRUE)),
                                     digits = -2)
    
  }
  
  # Return
  return(data)
  
}
