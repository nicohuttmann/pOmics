#' Adds group vector to data frame
#'
#' @param data_ data frame
#' @param which observations data
#' @param column.name name of new column
#' @param observations.set which observations.set to use
#' @param dataset dataset
#' @param input data frame to be modified
#' @param output data frame to return in list
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
include_observations_data <- function(data_,
                                      which,
                                      column.name,
                                      observations.set,
                                      dataset,
                                      input,
                                      output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }

  # Matrix to tibble
  if (!tibble::is_tibble(data))
    data <- data2tibble(data = data, row.names = "observations")


  # Check dataset
  if (!hasArg(dataset)) dataset <- attr(data, "dataset")
  dataset <- get_dataset(dataset)


  # Check observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)



  # Get groups data
  data.vector <- .datasets[[dataset]][["observations"]][[observations.set]] %>%
    dplyr::pull(var = !!dplyr::enquo(which), name = "observations")


  # Get observations
  observations <- dplyr::pull(data, var = observations)


  # Check observations and groups
  if (any(!observations %in% names(data.vector)))
    stop("Data does not contain all observations.")


  # Remove observations
  data.vector <- data.vector[observations]


  # Use input as name for new column
  if (!hasArg(column.name)) {

    if (!tryCatch(is.character(which),
                  error = function(cond) FALSE)) {

      column.name <- deparse(substitute(which))

    } else {

      column.name <- which

    }

  }


  # Add groups
  data <- data %>%
    dplyr::mutate(!!column.name := data.vector, .after = observations)



  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
