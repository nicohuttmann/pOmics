#' Allows to manually change the variables data
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>% 
#'
change_variables_manually <- function(dataset) {
  
  # Get dataset
  dataset <- get_dataset(dataset)
  
  x <- .datasets[[dataset]][["variables"]] %>% 
    edit() %>% 
    tibble::as_tibble()
  
  # 
  if (menu(choices = c("Yes", "No"), graphics = T, title = "Accept changes?") == 1) {
    
    .datasets[[dataset]][["variables"]] <<- x
    
  }
  
}
