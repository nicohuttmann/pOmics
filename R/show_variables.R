#' Prints variables data frame
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
show_variables <- function(dataset) {
  
  # get_dataset
  dataset <- get_dataset(dataset)
  
  # print
  print(.datasets[[1]][["variables"]])
  
  
}
