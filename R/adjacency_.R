#' Adds adjacency to cor_list
#'
#' @param cor_list cor_list object
#' @param method adjacency method
#' @param alpha shape of sigmoid function
#' @param theta turning point of sigmoid function
#' @param beta exponent of power function
#'
#' @return
#' @export
#'
#'
adjacency_ <- function(cor_list, method = "power", alpha = 2, theta = 0.5, beta = 1) {
  
  # Test cor_list
  if (!is.list(cor_list) || length(cor_list) == 0 || !"similarity" %in% names(cor_list)) stop("cor_list object seems corrupted.")
  
  # Add similarity 
  cor_list[["adjacency"]] <- adjacency(data = cor_list[["similarity"]],
                                        method = method,
                                        alpha = alpha,
                                        theta = theta,
                                        beta = beta)
  
  # Add attribute
  attr(cor_list, "adjacency") <- method
  
  # Return list
  return(cor_list)
  
}
