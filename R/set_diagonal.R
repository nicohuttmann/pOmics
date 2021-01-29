#' Sets diagonal values of symmetric matrix
#'
#' @param data data
#' @param diag new value for diagonal values
#'
#' @return
#' @export
#'
#'
set_diagonal <- function(data, diag = 1) {
  
  # Set diagonal values
  diag(data) <- diag
  
  # Return
  return(data)
  
}
