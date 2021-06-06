#' Removes columns from data frame based on minimum values above 0
#'
#' @param data_ data
#' @param min min. fraction of values above 0
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#'
remove_variables <- function(data_, min = 0.5, input = "raw_data", output = "data") {
  
  
  # Check input
  if (!hasArg(data_)) stop("No data given.")
  
  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)
  
  #
  if (list.input & !input %in% names(data_)) stop("Data could not be found. Please specify correct <input>.")
  
  
  # Get data
  if (list.input) data <- data_[[input]]
  
  else data <- data_
  
  
  
  # Remove columns
  data <- dplyr::select(data, -where(function(x) is.numeric(x) & mean(x > 0) < min))
  
  
  
  # Prepare return
  if (list.input) data_[[output]] <- data
  
  else data_ <- data
  
  # Return
  return(data_)
  
}
