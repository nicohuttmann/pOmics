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
impute_missForest <- function(data_, input, output, ...) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  # Impute by missForest
  data <- data %>%
    eval_data(expr = ifelse(x > 0, x, NA)) %>%
    tibble2matrix() %>%
    missForest::missForest(...) %>%
    purrr::pluck("ximp") %>%
    matrix2tibble()


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
