#' Helper function to combine technical replicates (only returns mean if all values are > 0)
#'
#' @param x numeric vector
#'
#' @return
#' @export
#'
#' 
mean_or_0 <- function(x) {
  
  # Return 0 if any entry is 0, otherwise the mean of the number
  return(ifelse(any(x == 0), 0, mean(x)))
  
}
