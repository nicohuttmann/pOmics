#' Removes columns from data frame based on minimum values above 0
#'
#' @param data_ data
#' @param min min. fraction of values above 0
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#'
remove_variables <- function(data_, min = 0.5, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  # Remove columns

  keep <- colnames_class(data, c("character", "factor"))

  keep <- c(keep, data %>%
              dplyr::select(colnames_class(., "numeric")) %>%
              sapply(function(x) mean(x > 0) >= min) %>%
              which_names())

  data <- dplyr::select(data, all_of(keep))


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
