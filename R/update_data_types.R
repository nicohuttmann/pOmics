#' Adds new data type
#'
#' @param dataset dataset
#' @param default.type new default data type
#'
#' @return
#' @export
#'
#'
update_data_types <- function(dataset, default.type = NA) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Update
  attr(.datasets[[dataset]], "data_types") <- setdiff(names(.datasets[[dataset]]), c("variables", "observations"))


  # Set default if it's the first dataset
  if (!is.na(default.type)) {
    #
    set_default_data_type(type = default.type, dataset = dataset, silent = TRUE)
  } else if (is.na(attr(.datasets[[dataset]], "default_data_type"))) {
    #
    set_default_data_type(type = get_data_types(dataset = dataset, print = FALSE, return = TRUE)[1], dataset, silent = TRUE)
  }


}
