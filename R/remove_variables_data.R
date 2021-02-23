#' Removes variables data
#'
#' @param name names of columns to be removed
#' @param dataset dataset
#' @param require.confirmation Require additional confirmation before execution
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
remove_variables_data <- function(name, dataset, require.confirmation = T) {

  # Check dataset
  dataset <- get_dataset(dataset = dataset)

  # Check name/s
  if (!hasArg(name)) {

    # Choose dataframe
    columns <- select.list(choices = colnames(.datasets[[dataset]][["variables"]]),
                           multiple = TRUE,
                           title = "Choose columns to remove from variables data: ",
                           graphics = TRUE)

    # Check if any column has been selected
    if (length(columns) == 0) stop("No columns selected.")

  # Check given names vector
  } else if (any(!name %in% colnames(.datasets[[dataset]][["variables"]]))) {

    # Intersect given names and present names
    columns <- intersect(name, colnames(.datasets[[dataset]][["variables"]]))

    # Do any given column names match
    if (length(name) == 0) stop("No given names match column names.")

    message("Following columns will be removed: ")
    print(name)

  } else {
    columns <- name
  }



  # Eventually ask for confirmation
  if (require.confirmation) {

    do <- utils::askYesNo(msg = "Remove selected columns?")

    if (!is.na(do) || do) {

      # Remove columns
      .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
        dplyr::select(-all_of(columns))
    }

  # Reassign dataframe
  } else {

    # Remove columns
    .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
      dplyr::select(-all_of(columns))

  }

}
