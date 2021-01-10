#' Check data type and return data.name
#'
#' @param dataset datset
#' @param type type of data
#' @param data.name (optional) name of data
#'
#' @return
#' @export
#'
#'
get_data_name <- function(dataset, type, data.name) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Stop if data type is not in dataset
  if (!type %in% attr(.datasets[[dataset]], "data.types")) stop("Data type not in dataset.")

  # No data.name specified; returns defaults data.name for given type
  if (!hasArg(data.name)) return(attr(.datasets[[dataset]], paste0("default_", type)))

  # Returns data.name if found
  if (data.name %in% attr(.datasets[[dataset]], type)) return(data.name)

  # Not found; no number identification
  stop("Given data name incorrect.")

}
