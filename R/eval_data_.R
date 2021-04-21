#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as variable)
#' @param FUN function to be applied to each cell
#' @param data.name name of data to use
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
eval_data_ <- function(data_, expr, FUN, data.name = "raw_data") {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!data.name %in% names(data_)) stop("Data could not be found. Please specify correct <data.name>.")


  # Get data
  data <- data_[[data.name]]

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


  data_[[data.name]] <- data



  # Return
  invisible(data_)

}
