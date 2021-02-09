#' Removes proteins based on prefixes from vector
#'
#' @param vector vector
#' @param prefix prefix to be removed
#'
#' @return
#' @export
#'
#'
cleanup_proteins_v <- function(vector, prefix = c("CON_", "REV_")) {

  # Remove proteins from vector based on prefix
  for (i in prefix) {

    vector <- vector[!grepl(i, vector)]

  }

  # Return
  return(vector)

}
