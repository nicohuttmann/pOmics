#' Returns names vector from data frame
#'
#' @param data data frame, tibble or matrix
#' @param values column for vector values; by default second column
#' @param names column for vector names; by default first column
#'
#' @return
#' @export
#'
#' 
data2vector <- function(data, values, names) {
  
  # Check input
  if (!hasArg(data)) {
    
    cat("No data frame given.")
    
    invisible(NULL)
    
  } 
  
  # Check values and names column
  if (!hasArg(values)) 
    values <- 2
  
  if (!hasArg(names)) 
    names <- 1
  
  # Transform DF to tibble
  return(data %>% 
    data2tibble() %>% 
    dplyr::pull(var = !!values, name = !!names))
  
}
