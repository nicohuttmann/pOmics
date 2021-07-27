#' Collapses rows of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups
#' @param name name of new column
#' @param input name of input data frame
#' @param new.name name of output data frame (defaults to input data name)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
collapse_columns <- function(data_, FUN = sum, name, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  # Name of new column
  if (!hasArg(name)) {
    name <- deparse(substitute(FUN))
  }


  # Summarise data
  data <- data %>%
    dplyr::rowwise() %>%
    mutate(count = FUN(c_across(cols = where(is.logical) | where(is.logical))),
           .before = where(is.logical) | where(is.logical))





  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
