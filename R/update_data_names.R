#' Add new data name
#'
#' @param type data type
#' @param dataset dataset
#' @param default.name new default name
#'
#' @return
#' @export
#'
#'
update_data_names <- function(type, dataset, default.name = NA) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # check data type
  type <- get_data_type(type, dataset)


  # Update data names
  attr(.datasets[[dataset]], type) <- names(.datasets[[dataset]][[type]])


  # Set default if it is the first dataset
  if (!is.na(default.name)) {
    #
    set_default_data_name(default.name, type, dataset, silent = T)
  } else if(is.na(attr(.datasets[[dataset]], paste0("default_", type))))
    #
    set_default_data_name(name = get_data_names(type = type, dataset = dataset, print = F, return = T)[1],
                          type = type,
                          dataset = dataset,
                          silent = T)

}
