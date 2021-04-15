#' Adds group vector to data frame
#'
#' @param analysis_list analysis list
#' @param groups groups
#' @param control (optional) which group is control
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
include_groups_ <- function(analysis_list, groups, control, observations.set, dataset, target.data = "raw_data") {

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
  group.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!dplyr::enquo(groups), name = "observations")


  # Get observations
  observations <- dplyr::pull(data, var = observations)


  # Check observations and groups
  if (any(!observations %in% names(group.vector))) stop("Groups do not contain all observations.")


  # Remove observations
  group.vector <- group.vector[observations]




  # Define order of groups

  # More than two groups
  if (!hasArg(control)) {

    group.factors <- factor(group.vector)

    # control defined but not in groups
  } else if (hasArg(control) && !(control %in% group.vector)) {

    stop("Control not found in groups.")

    # Control group defined
  } else if(hasArg(control) && control %in% group.vector) {

    group.factors <- factor(group.vector, levels = c(control, setdiff(unique(group.vector), control)))

    #
  } else {
    stop("Something went wrong.")
  }

  # Add groups
  analysis_list[[target.data]] <- data %>%
    dplyr::mutate(groups = group.factors, .after = observations)


  # Return
  return(analysis_list)

}
