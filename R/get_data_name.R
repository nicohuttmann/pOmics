#' Check data type and return data.name
#'
#' @param name (optional) name of data
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_data_name <- function(name, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Default if not given
  if (!hasArg(name)) return(get_default_data_name(dataset))

  # Name correct
  if (is_data_name(name, dataset)) return(name)

  else stop("Dataset could not be found.")

}
