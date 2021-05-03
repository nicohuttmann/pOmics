#' Returns variables data names
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_variables_data_names <- function(dataset) {

  # get_dataset
  dataset <- get_dataset(dataset)

  # print
  return(names(.datasets[[dataset]][["variables"]]))

}
