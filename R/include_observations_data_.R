#' Adds group vector to data frame
#'
#' @param data data frame
#' @param name observations data
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param target.data data frame to be modified
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_observations_data_ <- function(analysis_list, name, observations.set, dataset, target.data = "raw_data") {

  # Check data input
  if (!hasArg(analysis_list)) stop("Please provide data input.")

  if (!target.data %in% names(analysis_list)) stop("Target data frame not found.")

  # Extract data
  data <- analysis_list[[target.data]]

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
  if (!tryCatch(is.character(name),
               error = function(cond) FALSE)) {

    name <- deparse(substitute(name))

  }


  # Add groups
  analysis_list[[target.data]] <- data %>%
    dplyr::mutate(!!name := data.vector, .after = observations)


  # Return
  return(analysis_list)

}
