#' Set default data name
#'
#' @param name data name
#' @param type data type
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
set_default_data_name <- function(name, type, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # check data type
  type <- get_data_type(type)

  # Change default
  set_dataset_attr(x = name, which = paste0("default_", type), dataset = dataset)

}
