#' Adds new datasets to list
#'
#' @param name dataset to add
#'
#' @return
#' @export
#'
#'
add_dataset <- function(name) {

  if (hasArg(name)) {

    # .datasets file
    new_datasets_list()

    # .info file
    new_info_list()


    # Check if name is already present in datasets
    if (name %in% names(.datasets)) {
      message("Dataset or name already added.")
      return(invisible(FALSE))
    }


    # Get raw dataset for attributes
    raw_dataset <- .info[["raw_datasets"]][[name]]

    # Build dataset list
    # Start list
    dataset <- tibble::lst()


    # Preset attributes
    attr(dataset, "name") <- attr(raw_dataset, "name")
    attr(dataset, "project") <- attr(raw_dataset, "project")
    attr(dataset, "data.type") <- attr(raw_dataset, "data.type")
    attr(dataset, "path") <- attr(raw_dataset, "path")
    attr(dataset, "time") <- attr(raw_dataset, "time")
    attr(dataset, "separator") <- attr(raw_dataset, "separator")

    attr(dataset, "taxId") <- NA
    attr(dataset, "species") <- NA


    attr(dataset, "default_variables") <- "All"
    attr(dataset, "default_variables_labels") <- "variables"

    attr(dataset, "default_observations_set") <- "raw"
    attr(dataset, "default_observations") <- "All"
    attr(dataset, "default_observations_labels") <- "observations"
    attr(dataset, "default_groups") <- NA

    attr(dataset, "default_data_name") <- NA

    # Add dataset
    .datasets[[name]] <<- dataset


    # Add default data set
    if (is.na(get_default_dataset())) {

      set_default_dataset(name)

    }


    # Indicate if new info list was created
    return(invisible(TRUE))

  } else {

    message("No dataset name provided.")
    return(invisible(FALSE))
  }

}
