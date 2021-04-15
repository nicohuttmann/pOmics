#' Adds group vector to data frame
#'
#' @param data data frame
#' @param groups groups
#' @param control (optional) which group is control
#' @param observations.set which observations.set to use
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_groups <- function(data, groups, control, observations.set, dataset) {

  # Check data input
  if (!hasArg(data)) stop("Please provide data input.")

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
  data <- data %>%
    dplyr::mutate(groups = group.factors, .after = observations)


  # Return
  return(data)

}
