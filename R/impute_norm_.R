#' Imputes values based on normal deviation
#'
#' @param analysis_list analysis list
#' @param shift shift of normal distribution in standard deviations
#' @param width width of normal distribution in standard deviations
#' @param seed seed for reproducibility
#' @param target.data name of data to be modified
#'
#' @return
#' @export
#'
#'
impute_norm_ <- function(analysis_list, shift = 1.8, width = 0.3, seed = 123, target.data = "raw_data") {
  
  # Check input
  if (!hasArg(analysis_list) |
      !(is.list(analysis_list)) |
      is.data.frame(analysis_list) |
      tibble::is_tibble(analysis_list) |
      is.matrix(analysis_list) |
      !target.data %in% names(analysis_list))
    stop("Please provide a list containg your indicated data as input.")
  
  
  # Extract data frame
  data <- analysis_list[[target.data]]
  
  # Impute values
  data <- impute_norm(data = data, shift = shift, width = width, seed = seed)
  
  # Readd data
  analysis_list[[target.data]] <- data
  
  # Return
  return(analysis_list)
  
}
