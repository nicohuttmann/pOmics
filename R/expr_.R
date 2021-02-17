#' Title
#'
#' @param data 
#' @param groups 
#' @param scale 
#' @param combine 
#' @param observations.set 
#' @param dataset 
#'
#' @return
#' @export
#'
#'
expr_ <- function(data, groups, scale = T, combine = "mean", observations.set, dataset) {
  
  # Setup a list to store all correlation data
  expr_list <- tibble::lst()
  
  # Add raw data
  expr_list[["raw_data"]] <- data
  
  # Add variable names
  attr(expr_list, "variables") <- colnames(data)
  
  
  
  # Get groups
  if (hasArg(groups)) {
    groups <- get_groups(observations = rownames(data),
                         groups = !!dplyr::enquo(groups),
                         observations.set = observations.set,
                         dataset = dataset)
  }
  
  
  # Scale (Z-scores)
  if (scale) data <- scale(data)
  
  data <- data[, apply(data, MARGIN = 2, function(x) !any(is.na(x)))]
  
  
  # Combine groups
  if (hasArg(groups)) data <- combine_data_groups(data = data, groups = groups, method = combine)
  
  
  # Add raw data
  expr_list[["data"]] <- t(data)
  
  
  # Return
  return(expr_list)
  
}
