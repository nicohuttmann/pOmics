#' Returns collective variables from all datasets
#'
#' @return
#' @export
#'
#'
get_all_variables <- function() {
  
  # 
  variables <- c()
  
  # 
  for (dataset in get_datasets(print = FALSE, return = TRUE)) {
    variables <- unique(c(variables, get_variables(variables = all,
                                                   dataset = dataset)))
  }
  
  
  return (variables)
  
}