#' Removes proteins based on prefixes from columns
#'
#' @param matrix data
#' @param prefix prefix to be removed
#'
#' @return
#' @export
#'
#'
cleanup_proteins_m <- function(matrix, prefix = c("CON_", "REV_")) {

  # Remove proteins from dataframe based on prefix
  for (i in prefix) {

    matrix <- matrix[, !grepl(i, colnames(matrix))]

  }

  # Return
  return(matrix)

}
