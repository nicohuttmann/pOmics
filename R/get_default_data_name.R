#' Return default data name
#'
#' @param dataset datset
#'
#' @return
#' @export
#'
#'
get_default_data_name <- function(dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Return
  return(attr(.datasets[[dataset]], "default_data_name"))

}
