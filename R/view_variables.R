#' Prints variables data frame
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
view_variables <- function(dataset) {

  # get_dataset
  dataset <- get_dataset(dataset)

  # print
  print(.datasets[[dataset]][["variables"]])
  View(.datasets[[dataset]][["variables"]])


}
