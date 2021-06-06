#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param ... specific arguments
#' @param input name of input
#' @param output name of output
#'
#' @return
#' @export
#'
#'
do_nothing <- function(data_, ..., input = "raw_data", output = "data") {
  
  # Check input
  if (!hasArg(data_)) {
    
    message("No data given.")
    
    invisible(NULL)
    
  }
  
  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)
  
  # Check list input
  if (list.input & !input %in% names(data_)) {
    
    message("Data could not be found. Please specify correct <input>.")
    
    invisible(data_)
    
  }
  
  # Get data
  if (list.input) data <- data_[[input]]
  
  else data <- data_
  
  
  
  
  
  
  data <- data
  
  
  
  
  
  
  # Prepare return
  if (list.input) data_[[output]] <- data
  
  else data_ <- data
  
  # Return
  return(data_)
  
}
