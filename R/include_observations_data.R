#' Adds observations data to data frame
#'
#' @param data observations data
#' @param name name of data
#' @param observations.set which observations.set to use
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_observations_data <- function(data, name, observations.set, dataset) {

  # Check data input
  if (!hasArg(data)) stop("Please provide data input.")

  # Matrix to tibble
  if (!tibble::is_tibble(data)) data <- data2tibble(data = data, row.names = "observations")

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)



  # Get observations data
  data.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(!!dplyr::enquo(name))


  # Get observations
  observations <- dplyr::pull(data, var = observations)


  # Check observations and new data
  if (any(!observations %in% names(data.vector))) stop("New data does not contain all observations.")


  # Remove observations
  data.vector <- data.vector[observations]


  # Use input as name for new column
  if (!tryCatch(is.character(name),
                error = function(cond) FALSE)) {

    name <- deparse(substitute(name))

  }


  # Add name
  data <- data %>%
    dplyr::mutate(!!name := data.vector, .after = observations)


  # Return
  return(data)

}
