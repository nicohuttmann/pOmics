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
transpose_data <- function(data_, from.row.names, to.row.names, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  # Determine from rownames
  if (!hasArg(from.row.names)) from.row.names <- colnames(data)[1]

  # Determine to.row.names
  if (!hasArg(to.row.names)) {
    if (from.row.names == "observations") {
      to.row.names <- "variables"
    } else if (from.row.names == "variabels") {
      to.row.names <- "observations"
    } else {
      to.row.names <- "names"
    }
  }

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
