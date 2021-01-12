#' Check data type and return data.name
#'
#' @param name (optional) name of data
#' @param type type of data
#' @param dataset datset
#'
#' @return
#' @export
#'
#'
get_data_name <- function(name, type, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # check data type
  type <- get_data_type(type)

  # Default if not given
  if (!hasArg(name)) return(get_default_data_name(type, dataset))

  # Name correct
  if (is_data_name(name, type, dataset)) return(name)

  else stop("Dataset could not be found.")

}
