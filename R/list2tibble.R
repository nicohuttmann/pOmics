#' Transforms a list to tibble logical indication for variables
#'
#' @param x list
#'
#' @return
#' @export
#' 
#' @importFrom magrittr %>% 
#'
#'
list2tibble <- function(x, identifier = "variables") {
  
  # 
  df <- tibble::tibble(!!identifier := unique(unlist(x)))
  
  # Add logical columns
  for (column in names(x)) {
    df <- df %>%
      dplyr::mutate(!!column := variables %in% x[[column]])
  }
  
  # Return
  return(df)
  
}
