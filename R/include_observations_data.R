#' Adds group vector to data frame
#'
#' @param data_ data frame
#' @param name observations data
#' @param column.name name of new column
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param input data frame to be modified
#' @param output dataframe to return in list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_observations_data_ <- function(data_, name, column.name, observations.set, dataset,
                                       input = "LFQ.intensity", output) {

  # Check data input
  if (!hasArg(data_)) stop("Please provide data input.")

  if (!input %in% names(data_)) stop("Target data frame not found.")

  # Extract data
  data <- data_[[input]]

  # Matrix to tibble
  if (!tibble::is_tibble(data)) data <- data2tibble(data = data, row.names = "observations")

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)



  # Get groups data
  data.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!dplyr::enquo(name), name = "observations")


  # Get observations
  observations <- dplyr::pull(data, var = observations)


  # Check observations and groups
  if (any(!observations %in% names(data.vector))) stop("Groups do not contain all observations.")


  # Remove observations
  data.vector <- data.vector[observations]


  # Use input as name for new column
  if (!hasArg(column.name)) {

    if (!tryCatch(is.character(name),
                  error = function(cond) FALSE)) {

      column.name <- deparse(substitute(name))

    } else {

      column.name <- name

    }

  }


  if (!hasArg(output)) output <- input

  # Add groups
  data_[[output]] <- data %>%
    dplyr::mutate(!!column.name := data.vector, .after = observations)


  # Return
  return(data_)

}
