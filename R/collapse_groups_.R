#' Collapses groups of data frame by specified function and adds as list entry
#'
#' @param analysis_list analysis_list
#' @param FUN function to combine groups
#' @param group.column column specifying the groups
#' @param data.name name of data frame entry
#' @param new.name new entry name
#'
#' @return
#' @export
#' 
#' @importFrom magrittr %>% 
#'
#'
collapse_groups_ <- function(analysis_list, FUN = mean, group.column = "groups", data.name = "data", new.name = "grouped_data") {
  
  # Check input
  if (!hasArg(analysis_list) | !is.list(analysis_list) | !tibble::is_tibble(analysis_list[[data.name]])) stop("Please provide a list with the respective tibble.")
  
  if (!group.column %in% names(analysis_list[[data.name]])) stop("Group column not found in data frame.")
  
  # Add new data frame
  analysis_list[[new.name]] <- analysis_list[[data.name]] %>% 
    dplyr::group_by(rlang::eval_tidy(rlang::parse_expr(group.column))) %>% 
    dplyr::summarise(across(.cols = where(is.numeric), .fns = FUN)) %>% 
    dplyr::rename(!!group.column := "rlang::eval_tidy(rlang::parse_expr(group.column))")
  
  # Return
  return(analysis_list)
  
}
