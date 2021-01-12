#' Set default data type
#'
#' @param type data type
#' @param dataset dataset
#' @param silent Silent?
#'
#' @return
#' @export
#'
#'
set_default_data_type <- function(type, dataset, silent = T) {

  # Check dataset
  dataset <- get_dataset(dataset)


  # Add if name is in list
  if (type %in% attr(.datasets[[dataset]], "data_types")) {
    #
    attr(.datasets[[dataset]], "default_data_type") <<- type
    if (!silent) message(paste0(attr(.datasets[[dataset]], "default_data_type"), " was set as default data type."))

  } else {

    if (!silent) message("Default data type not changed.")

  }

}
