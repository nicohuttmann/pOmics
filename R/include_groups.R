#' Adds group vector to data frame
#'
#' @param data_ analysis list
#' @param groups groups
#' @param control (optional) which group is control
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_groups <- function(data_, groups = "default", control, observations.set, dataset, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }

  # Matrix to tibble
  if (!tibble::is_tibble(data)) data <- data2tibble(data = data, row.names = "observations")

  # Check dataset
  dataset <- get_dataset(dataset)

  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)


  # Default groups
  if (groups == "default") {

    groups <- get_dataset_attr(which = "default_groups", dataset = dataset)

    if (is.null(groups)) {

      stop("Setup default groups data or provide a groups argument.")

    }

  }



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


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
