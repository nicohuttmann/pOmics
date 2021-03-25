#' Scales data form tibbles or analysis_list containing a tibble
#'
#' @param analysis_list data list
#' @param data.name name of data entry
#' @param new.name new name of scaled data
#'
#' @return
#' @export
#' 
#' @importFrom magrittr %>% 
#'
#' 
scale_ <- function(analysis_list, data.name = "data", new.name = "data") {
  
  # Data frame input
  if (tibble::is_tibble(analysis_list)) return(analysis_list %>%
                                                 dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = scale)))
  
  else {
    
    # check data
    if (!hasArg(analysis_list) | !is.list(analysis_list) | !data.name %in% names(analysis_list)) 
      stop("Please provide a list with the specified data entry.")
    
    # Scale data
    analysis_list[[new.name]] <- analysis_list[[data.name]] %>% 
      dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = scale))
    
    # Return
    return(analysis_list)
    
  }
 
}
