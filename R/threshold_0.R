#' Removes columns from data frame based on minimum values above 0
#'
#' @param data data
#' @param min min. fraction of values above 0
#' @param groups (optional) groups
#'
#' @return
#' @export
#'
#'
threshold_0 <- function(data, min = 0.5, groups) {
  
  # Get groups
  #groups <- get_groups()
  
  
  if (!hasArg(groups)) {
    
    data <- data[, apply(X = data,
                         MARGIN = 2,
                         FUN = function(x) mean(x > 0) >= min)]
  }
  
  # Return
  return(data)
  
}
