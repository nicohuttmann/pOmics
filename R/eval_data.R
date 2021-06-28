#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as variable, e.g. x > 2)
#' @param FUN function to be applied to each cell
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
eval_data <- function(data_, expr, FUN, input = "Peptides", output) {

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





  # Save dimensions of data frame
  dimension <- dim(data)


  # Apply defined expr or function
  if (hasArg(expr)) {

    data <- data %>%
      dplyr::mutate(dplyr::across(where(is.numeric), function(x) rlang::eval_tidy(rlang::enexpr(expr))))

  # Apply function
  } else if (hasArg(FUN)) {

    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(where(is.numeric), FUN))

  } else {

    stop("Please provide either an expression <expr> or a function <FUN> to evaluate the data.")

  }


  # Check dimensions after data manipulation
  if (!all(dimension == dim(data))) {

    message("Attention: The applied function changed the dimensions of the data.")

  }



  # Prepare return
  if (list.input) data_[[ifelse(hasArg(output), output, input)]] <- data

  else data_ <- data

  # Return
  return(data_)

}
