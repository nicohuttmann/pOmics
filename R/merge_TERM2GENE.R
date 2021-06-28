#' Combines TERM2GENE dataframes
#'
#' @param ... TERM2GENE dataframes
#'
#' @return
#' @export
#'
#'
merge_TERM2GENE <- function(...) {
  
  data <- list(...)
  
  
  data <- lapply(data, function(x) {
    colnames(x) <- c("TERM", "GENE")
    return(x)})
  
  
  if (length(data) < 2) return(data[[1]])
  
  return <- data[[1]]
  
  for (i in 2:length(data)) {
    
    return <- rbind(return, data[[i]])
    
  }
  
  # Return
  return(return)
  
}
