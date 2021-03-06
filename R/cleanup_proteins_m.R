#' Removes proteins based on prefixes from columns
#'
#' @param matrix data
#' @param prefix prefix to be removed
#'
#' @return
#' @export
#'
#'
cleanup_proteins_m <- function(matrix, prefix = c("CON_", "REV_", "NA_")) {

  # Remove proteins from dataframe based on prefix
  for (i in prefix) {

    matrix <- matrix[, regexpr(i, colnames(matrix)) != 1]

  }

  # Return
  return(matrix)

}
