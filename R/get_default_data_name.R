#' Return default data name
#'
#' @param type data type
#' @param dataset datset
#'
#' @return
#' @export
#'
#'
get_default_data_name <- function(type, dataset) {

  # check dataset
  dataset <- get_dataset(dataset)

  # Check type
  type <- get_data_type(type)


  # Return
  return(attr(.datasets[[dataset]], paste0("default_", type)))

}
