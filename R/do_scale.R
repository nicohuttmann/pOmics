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

  else data <- input_list[["data"]]



  # Scale data
  data <- data %>%
    dplyr::mutate(dplyr::across(.cols = where(is.numeric),
                                .fns = function(x) c(scale(x)))) %>%
    dplyr::mutate(dplyr::across(.cols = where(function(x) all(is.na(x))),
                                .fns = function(x) 0))



  # Output name
  if (!hasArg(output)) output <- input_list[["input"]]

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
