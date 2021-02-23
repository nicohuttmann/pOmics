#' Removes observations data
#'
#' @param name names of columns to be removed
#' @param observations.set observations.set
#' @param dataset dataset
#' @param require.confirmation Require additional confirmation before execution
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
remove_observations_data <- function(name, observations.set, dataset, require.confirmation = T) {

  # Check dataset
  dataset <- get_dataset(dataset = dataset)

  # Check observatoins set
  observations.set <- get_observations_set(observations.set, dataset)

  # Check name/s
  if (!hasArg(name)) {

    # Choose dataframe
    columns <- select.list(choices = colnames(.datasets[[dataset]][["observations"]][[observations.set]]),
                           multiple = TRUE,
                           title = "Choose columns to remove from observations data: ",
                           graphics = TRUE)

    # Check if any column has been selected
    if (length(columns) == 0) stop("No columns selected.")

    # Check given names vector
  } else if (any(!name %in% colnames(.datasets[[dataset]][["observations"]][[observations.set]]))) {

    # Intersect given names and present names
    columns <- intersect(name, colnames(.datasets[[dataset]][["observations"]][[observations.set]]))

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
      .datasets[[dataset]][["observations"]][[observations.set]] <<-
        .datasets[[dataset]][["observations"]][[observations.set]] %>%
        dplyr::select(-all_of(columns))
    }

    # Reassign dataframe
  } else {

    # Remove columns
    .datasets[[dataset]][["observations"]][[observations.set]] <<-
      .datasets[[dataset]][["observations"]][[observations.set]] %>%
      dplyr::select(-all_of(columns))

  }

}
