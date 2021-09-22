#' Adds group vector to data frame
#'
#' @param data_ data frame
#' @param which variables data
#' @param column.name name of new column
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
include_variables_data <- function(data_,
                                   which,
                                   column.name,
                                   dataset,
                                   input,
                                   output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }

  # Matrix to tibble
  if (!tibble::is_tibble(data)) data <- data2tibble(data = data,
                                                    row.names = "variables")

  # Check dataset
  dataset <- get_dataset(dataset)



  # Get variables data
  data.vector <- .datasets[[dataset]][["variables"]]%>%
    dplyr::pull(var = !!dplyr::enquo(which), name = "variables")


  # Get variables
  variables <- dplyr::pull(data, var = variables)


  # Check variables and groups
  if (any(!variables %in% names(data.vector)))
    stop("Data does not contain all variables.")


  # Remove variables
  data.vector <- data.vector[variables]


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
    dplyr::mutate(!!column.name := data.vector, .after = variables)



  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
