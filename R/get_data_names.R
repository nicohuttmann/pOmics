#' Prints or returns data names
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_data_names <- function(dataset) {

  # dataset
  dataset <- get_dataset(dataset)

  # Return
  return(setdiff(names(.datasets[[dataset]]), c("variables", "observations")))

}
