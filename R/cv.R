#' Calculated coefficient of variation
#'
#' @param data data matrix
#'
#' @return
#' @export
#'
#'
cv <- function(data) {
  
  #
  cvs <- apply(X = data,
               MARGIN = 2,
               FUN = function(x) sd(x) / mean(x))
  
  # Return
  return(cvs)
  
}
