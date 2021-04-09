#' Set default data name
#'
#' @param name data name
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
set_default_data_name <- function(name, dataset) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # Change default
  set_dataset_attr(x = name, which = "default_data_name", dataset = dataset)

}
