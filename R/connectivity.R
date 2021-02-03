#' Variable-wise connectivity
#'
#' @param adjacency.matrix adjacency matrix
#' @param scale should connectivity be divided by n - 1
#'
#' @return
#' @export
#'
#'
connectivity <- function(adjacency.matrix, scale = T) {
  
  # Set diagonal to 0
  adjacency.matrix <- set_diagonal(data = adjacency.matrix, diag = 0)
  
  # Sum coefficients
  con <- apply(X = adjacency.matrix, MARGIN = 2, FUN = sum)
  
  # Scale
  if (scale) con <- con / (ncol(adjacency.matrix) - 1)
  
  # Return
  return(con)
  
}
