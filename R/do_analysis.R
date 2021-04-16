#' Performs analysis
#'
#' @param data_ data list
#' @param data.name name of data to use
#' @param plot results
#'
#' @return
#' @export
#'
#' 
do_analysis <- function(data_, data.name = "raw_data", plot = T) {
  
  # Check input
  if (!hasArg(data_)) stop("No data list given.")
  
  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")
  
  # Check data list
  if (!data.name %in% names(data_)) stop("Data could not be found. Please specify correct 'data.name'.")
  
  
  # Get data
  data <- data_[[data.name]]
  
  
  
  # Plot results
  if (plot) data_[["plot"]] <- plot(data)
  
  # Return
  invisible(data_)
  
}