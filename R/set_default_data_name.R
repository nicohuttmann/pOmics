#' Set default data name
#'
#' @param name data name
#' @param type data type
#' @param dataset dataset
#' @param silent Silent?
#'
#' @return
#' @export
#'
#'
set_default_data_name <- function(name, type, dataset, silent = T) {

  # Check dataset
  dataset <- get_dataset(dataset)

  # check data type
  type <- get_data_type(type)


  #Add if name is in list
  if (name %in% attr(.datasets[[dataset]], type)) {
    #
    attr(.datasets[[dataset]], paste0("default_", type)) <<- name
    if (!silent) message(paste0(attr(.datasets[[dataset]], paste0("default_", type)), " was set as default data name."))

  } else {

    if (!silent) message("Default data name not changed.")

  }

}
