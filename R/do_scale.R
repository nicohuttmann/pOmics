#' Scales data (Z-scores) from tibbles or analysis_list containing a tibble
#'
#' @param data_ data_ list
#' @param input name of input data
#' @param output name of output data
#'
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
do_scale <- function(data_, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }



  # Scale data
  data <- data %>%
    dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = scale))



  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
