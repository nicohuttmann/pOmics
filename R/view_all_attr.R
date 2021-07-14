#' Prints default dataset attributes
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' 
get_all_attr <- function(dataset) {
  
  # Dataset
  dataset <- get_dataset(dataset)
  
  # Collectr all attributes
  dataset.attr <- attributes(.datasets[[dataset]])
  
  # Print
  cat(paste0("Default settings for dataset ", dataset, ":\n\n"))
  
  print(dataset.attr)
  
}
