#' Imputes missing values based on a Random Forest algorithm
#'
#' @param data_ list or tibble
#' @param input name of input data
#' @param output name of output data
#' @param ... arguments for missForest function
#'
#' @return
#' @export
#'
#'
impute_missForest <- function(data_, input = "LFQ.intensity", output, ...) {

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    return(return(invisible(NULL)))

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    return(return(invisible(data_)))

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_


  # Impute by missForest
  data <- data %>%
    eval_data(expr = ifelse(x > 0, x, NA)) %>%
    tibble2matrix() %>%
    missForest::missForest(...) %>%
    purrr::pluck("ximp") %>%
    tibble2matrix()


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
