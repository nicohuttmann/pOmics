#' Returns default data type
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_default_data_type <- function(dataset) {

  # Check dataset
  dataset <- get_default(dataset)


  # Return
  return(attr(.datasets[[dataset]], "default_data_type"))

}
