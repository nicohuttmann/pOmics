#' Divides each row of a matrix by the elements of the specified row
#'
#' @param data expression data
#' @param row reference row
#'
#' @return
#' @export
#'
#'
factorize_matrix <- function(data, row) {
  
  if (!hasArg(data)) stop("No data given.")
  
  if (!hasArg(row)) {
    row <- 1
    message("Using row one as reference.")
  }
  
  t(t(data) / data[row, ])
  
}
