#' Check and get data type
#'
#' @param type data type
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_data_type <- function(type, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Default if not given
  if (!hasArg(type)) return(get_default_data_type(dataset))

  # Name correct
  if (is_data_type(type)) return(type)

  else stop("Dataset could not be found.")

}
