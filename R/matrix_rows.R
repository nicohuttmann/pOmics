#' Subsets matrix by rownames and checks for transformations when only one row is selected
#'
#' @param matrix matrix
#' @param row.names row naems
#'
#' @return
#' @export
#'
#'
matrix_rows <- function(matrix, row.names) {
  #
  dim <- dim(matrix)
  rows <- rownames(matrix)
  #
  matrix <- as.matrix(matrix[row.names, ])
  # change back if
  if (nrow(matrix) == dim[2] && ncol(matrix) == 1) {
    colnames(matrix) <- rows[row.names]
    return(t(matrix))
  } else {
    return(matrix)
  }

}
