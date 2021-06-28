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
do_scale <- function(data_, input = "LFQ.intensity", output) {


  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    return(invisible(NULL))

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    return(invisible(data_))

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_



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
