#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param FUN function to apply to data
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_something <- function(data_, FUN, ..., dataset, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }

  # Test input function
  if (!hasArg(FUN) | !is.function(FUN)) {
    message("No function given for FUN argument.")
    return(invisible(data_))
  }

  # Apply function
  data <- FUN(data, ...)


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
