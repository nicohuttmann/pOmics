#' Add new data name
#'
#' @param name data name
#' @param type data type
#' @param dataset dataset
#' @param set.default Should data name be set as default
#'
#' @return
#' @export
#'
#'
update_data_names <- function(name, type, dataset, set.default = T) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # check data type
  type <- get_data_type(type, dataset)


  # First entry
  if (length(attr(.datasets[[dataset]], type)) == 1 && is.na(attr(.datasets[[dataset]], type))) {
    attr(.datasets[[dataset]], type) <<- name
    # Add
  } else {
    attr(.datasets[[dataset]], type) <<- c(attr(.datasets[[dataset]], type), name)
  }

  # Set default if it is the first dataset
  if (is.na(attr(dataset, paste0("default_", type))) || set.default)
    set_default_data_name(name, type, dataset, silent = T)

}
