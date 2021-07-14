#' Returns possible input names
#'
#' @param data_ data_
#'
#' @return
#' @export
#'
#' 
get_input <- function(data_) {
  
  # Check input
  if (!hasArg(data_) | (length(data_) < 1)) {
    
    message("No data given.")
    
    return(invisible(NULL))
    
  }
  
  # Dataset
  #dataset <- get_dataset(dataset = dataset, try.all = FALSE)
  
  # Determine input name  
  input.names <- rev(names(data_))
  
  # Return
  return(input.names[1])
  
}
