#' Handles list input for functions
#'
#' @param data_ data_
#' @param input name of input data
#'
#' @return
#' @export
#'
#'
data_input <- function(data_, input) {
  
  # List to return
  input_list <- list(data = NULL,
                     input = "input",
                     list.input = TRUE,
                     error = FALSE)
  
  # Check input
  if (!hasArg(data_)) {
    
    message("No data given.")
    
    input_list[["error"]] <- TRUE
    
    return(input_list)
    
  }
  
  
  # Check if list or data frame given
  list.input <- !is.data.frame(data_) & is.list(data_)
  
  # Input
  if (list.input) {
    
    if (!hasArg(input)) {
      
      input <- get_input(data_ = data_)
      
    }
    
    # Check list input
    if (list.input & !input %in% names(data_)) {
      
      message("Data could not be found. Please specify correct <input>.")
      
      input_list[["data"]] <- data_
      
      input_list[["error"]] <- TRUE
      
      return(invisible(input_list))
      
    }
    
    input_list[["data"]] <- data_[[input]]
    
    input_list[["input"]] <- input
    
  } else {
    
    input_list[["data"]] <- data_
    
    input_list[["list.input"]] <- FALSE
    
  }

  # Return
  return(input_list)
  
}
