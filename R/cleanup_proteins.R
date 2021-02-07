#' Removes proteins based on prefixes
#'
#' @param data data
#' @param prefix prefix to be removed
#'
#' @return
#' @export
#'
#'
cleanup_proteins <- function(data, prefix = c("CON_", "REV_")) {
  
  # Remove proteins from dataframe based on prefix
  for (i in prefix) {
    
    data <- data[, !grepl(i, colnames(data))]
    
  }
  
  # Return
  return(data)
  
}
