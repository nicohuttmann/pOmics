#' Return variables data
#'
#' @param name name of data
#' @param variables (optional) vector of variables or expression
#' @param dataset dataset
#' @param vector.names column to use as names for vector
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
pull_variables_data <- function(name, variables, dataset, vector.names = "variables") {
  
  # check dataset
  dataset <- get_dataset(dataset)
  
  
  # Variables
  
  # No variables defined
  if (!hasArg(variables)) {
    
    variables <- get_variables()
    
    # Variables defined
  } else {
    
    # Check if input is vector
    vector.input <- tryCatch(is.vector(variables),
                             error = function(cond) FALSE)
    
    # Default variables
    if (vector.input && length(variables) == 1 && variables == "default") {
      variables <- get_variables(variables = "default", dataset = dataset)
    }
    
    
    # if variables input is vector
    if (!vector.input) {
      variables <- get_variables(variables = !!dplyr::enquo(variables), dataset = dataset)
    }
    
  }
  
  
  data <- .datasets[[dataset]][["variables"]] %>%
    dplyr::pull(var = !!dplyr::enquo(name), name = !!dplyr::enquo(vector.names))
  
  return(data[variables])
  
}
