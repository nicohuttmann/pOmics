#' Adds new datasets to list
#'
#' @param dataset dataset to add
#' @param return Should TRUE be returned if dataset is added successfully
#'
#' @return
#' @export
#'
#'
add_dataset <- function(dataset, return = T) {

  if (hasArg(dataset)) {

    # .datasets file
    new_datasets_list()

    # .info file
    new_info_list()

    # Check for name
    if (is.null(attr(dataset, "name"))) {
      message("Dataset could not be identified.")
      return(FALSE)
    }

    # Check if name is already present in datasets
    if (attr(dataset, "name") %in% names(.datasets)) {
      message("Dataset or name already added.")
      return(FALSE)
    }

    # Add dataset
    .datasets[[attr(dataset, "name")]] <<- dataset

    # Update info
    update_datasets(default.dataset = attr(dataset, "name"))

    # Indicate if new info list was created
    if (return) return(TRUE)
  } else {

    message("No dataset provided.")
    if (return) return(FALSE)
  }

}
