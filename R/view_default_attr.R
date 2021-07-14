#' Prints default dataset attributes
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' 
view_default_attr <- function(dataset) {
  
  # Dataset
  dataset <- get_dataset(dataset)
  
  # Collectr all attributes
  dataset.attr <- attributes(.datasets[[dataset]])
  
  # Remove non-default attributes
  dataset.attr <- dataset.attr[grepl(pattern = "default_", x = names(dataset.attr))]
  
  # Print
  cat(paste0("Default settings for dataset ", dataset, ":\n\n"))
  
  print(dataset.attr)
  
}
