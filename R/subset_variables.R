#' Removes columns from data frame based on minimum values above 0
#'
#' @param data_ data
#' @param variables variables
#' @param dataset dataset
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#'
subset_variables <- function(data_, variables, dataset, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  # Keep all non-variables columns
  columns <- colnames(data)[!colnames(data) %in% get_all_variables()]

  # Add proteins


  # check dataset
  dataset <- get_dataset(dataset)


  # Variables

  # No variables defined
  if (!hasArg(variables)) {

    variables <- get_variables(dataset = dataset)

    # Variables defined
  } else {

    # Check if input is vector
    vector.input <- tryCatch(is.vector(variables),
                             error = function(cond) FALSE)

    # Default variables
    if (vector.input && length(variables) == 1 && variables == "default") {
      variables <- get_variables(variables = "default", dataset = dataset)
    }


    # if variables input is vector
    if (!vector.input) {
      variables <- get_variables(variables = !!dplyr::enquo(variables), dataset = dataset)
    }

  }

  variables <- intersect(variables, colnames(data))

  columns <- c(columns, variables)



  # Remove columns
  data <- dplyr::select(data, dplyr::all_of(columns))


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
