#' Collapses groups of data frame by specified function
#'
#' @param data tibble data frame
#' @param FUN function to combine groups
#' @param group.column column specifying the groups
#'
#' @return
#' @export
#' 
#' @importFrom magrittr %>% 
#'
#'
collapse_groups <- function(data, FUN = mean, group.column = "groups") {
  
  # Check input
  if (!hasArg(data) | !tibble::is_tibble(data)) stop("Please provide a tibble as data.")
  
  if (!group.column %in% names(data)) stop("Group column not found in data frame.")
  
  # Return
  return(data %>% 
    dplyr::group_by(rlang::eval_tidy(rlang::parse_expr(group.column))) %>% 
    dplyr::summarise(across(.cols = where(is.numeric), .fns = FUN)) %>% 
    dplyr::rename(!!group.column := "rlang::eval_tidy(rlang::parse_expr(group.column))"))
  
}
