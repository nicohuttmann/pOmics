#' Normalizes and returns data
#'
#' @param data data 
#' @param method method to use
#' @param data2 (optional) second data set from which normalization factors should be generated
#'
#' @return
#' @export
#'
#'
normalize <- function(data, method = "none", data2) {
  
  # 
  if (method == "none") return(data)
  
  # 
  else if (method == "pqn") return(pqn(data, data2))
  
  
  # 
  else stop("Normalisation method not found.")
  
}