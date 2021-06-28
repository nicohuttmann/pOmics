#' Transposes tibble and uses first column as column names
#'
#' @param data_ data_ list
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
transpose_data_ <- function(data_, from.row.names = "observations", to.row.names = "variables",
                            input = "Peptides", output) {


  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    invisible(NULL)

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    invisible(data_)

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_


  # Transpose data
  data <- transpose_tibble(tibble = data,
                           from.row.names = from.row.names,
                           to.row.names = to.row.names)


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)


}
