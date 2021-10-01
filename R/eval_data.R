#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
#' @param FUN function to be applied to each cell
#' @param modify specify which columns to modify (default: all)
#' provide either names of columns or function to identify them (
#' e.g. modify = is.numeric)
#' @param ignore columns to ignore while evaluating data (ignores)
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
eval_data <- function(data_, expr, FUN, modify, ignore, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used
  }



  # Save dimensions of data frame
  dimension <- dim(data)


  ### Define columns to modify

  # ignore
  if (!hasArg(ignore)) {
    ignore <- c("observations", "variables", "groups", "labels")
  }

  # modify
  if (!hasArg(modify)) {
    modify <- colnames(data)
  } else if (is.function(modify)) {
    modify <- which_names(unlist(lapply(data, FUN = modify)))
  }

  modify <- setdiff(modify, ignore)


  # Apply defined expr or function
  if (hasArg(expr)) {

    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dplyr::across(!!modify,
                      function(x) rlang::eval_tidy(rlang::enexpr(expr)))) %>%
      dplyr::ungroup()

  # Apply function
  } else if (hasArg(FUN)) {

    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(!!modify, FUN)) %>%
      dplyr::ungroup()

  } else {

    stop(paste0("Please provide either an expression <expr> or a function ",
                "<FUN> to evaluate the data."))

  }


  # Check dimensions after data manipulation
  if (!all(dimension == dim(data))) {

    message(paste0("Attention: The applied function changed the dimensions ",
                   "of the data."))

  }

  # Output name
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
