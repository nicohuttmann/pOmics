#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_nothing <- function(data_, ..., dataset, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }



  # Do literally nothing
  data <- data



  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- .set_data_attributes(data, data_attributes)
    attr(data_, "data") <- output
  }

  else data_ <- .set_data_attributes(data, data_attributes)

  # Return
  return(data_)

}


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param FUN function to apply to data
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_fun <- function(data_, FUN, ..., dataset, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }



  # Test input function
  if (!hasArg(FUN) | !is.function(FUN)) {
    message("No function given for FUN argument.")
    return(invisible(data_))
  }


  # Apply function
  data <- FUN(data, ...)


  # Reset data attributes
  data <- .set_data_attributes(data, data_attributes)

  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- .set_data_attributes(data, data_attributes)
    attr(data_, "data") <- output
  }

  else data_ <- .set_data_attributes(data, data_attributes)

  # Return
  return(data_)

}


#' Evaluates data cell-wise
#'
#' @param data_ data list
#' @param expr expression to be applied to each cell (must contain x as
#' variable, e.g. x > 2)
#' @param FUN function to be applied to each cell
#' @param modify (optional) character vector of columns to modify
#' @param ignore (optional) character vector of columns to ignore
#' @param input if data_ is list: name of data to use
#' @param output if data_ is list: name of output data to save in list under
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_expr <-function(data_, expr, FUN, modify, ignore, input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Save dimensions of data frame
  dimension <- dim(data)


  # Define columns to modify

  # ignore
  if (!hasArg(ignore)) {
    ignore <- c()
  }

  # modify
  if (!hasArg(modify)) {
    modify <- .data_columns(data_attributes)
  }

  modify <- setdiff(modify, ignore)


  # Apply defined expr or function
  if (hasArg(expr)) {

    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(modify),
                      function(x) rlang::eval_tidy(rlang::enexpr(expr)))) %>%
      dplyr::ungroup()

    # Apply function
  } else if (hasArg(FUN)) {

    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(modify), FUN)) %>%
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

  # Reset data attributes
  data <- .set_data_attributes(data, data_attributes)


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- .set_data_attributes(data, data_attributes)
    attr(data_, "data") <- output
  }

  else data_ <- .set_data_attributes(data, data_attributes)

  # Return
  return(data_)

}


#' Transposes tibble and uses first column as column names
#'
#' @param data_ data_ list
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
do_transpose <- function(data_,
                         from.row.names,
                         to.row.names,
                         input,
                         output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Transpose data
  data <- transpose_tibble(tibble = data,
                           from.row.names = from.row.names,
                           to.row.names = to.row.names)


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




#' Collapses rows of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups
#' @param by column specifying the groups
#' @param input name of input data frame
#' @param new.name name of output data frame (defaults to input data name)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
do_row_summary <- function(data_, FUN = mean, by = "All", input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }

  # Add dummy variable for all
  if (by == "All" & !(by %in% names(data)))
    data <- dplyr::mutate(data, All = "all")
  if (!by %in% names(data)) {

    message("Group column not found in data frame.")
    return(invisible(data_))

  }


  data_columns <- .data_columns(data_attributes)

  # Keep initial data frame for later
  data0 <- data

  # Add new data frame
  data <- data %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = dplyr::all_of(data_columns),
                            .fns = FUN))

  # Collapse character and
  data0 <- data0 %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = -dplyr::any_of(data_columns),
                            .fns = function(x) if (length(unique(x)) == 1) x[1]
                            else NA)) %>%
    dplyr::select(where(~!any(is.na(.))))

  # Combine data frames
  data <- dplyr::full_join(data0, data, by = by)

  # Rename grouping column
  data <- data %>%
    dplyr::rename(!!data_attributes[["rows"]] := !!by)


  # Add new observations to data attributes
  data_attributes[["observations"]] <- data$observations
  data_attributes[["observations_data"]] <-
    setdiff(data_attributes[["observations_data"]], by)

  # Reset attributes
  data <- .set_data_attributes(data, data_attributes)


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- .set_data_attributes(data, data_attributes)
    attr(data_, "data") <- output
  }

  else data_ <- .set_data_attributes(data, data_attributes)

  # Return
  return(data_)

}


#' Collapses rows of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups with (default sum)
#' @param name (optional) name of new column
#' @param modify (optional) character vector of columns to modify
#' @param ignore (optional) character vector of columns to ignore
#' @param input name of input data frame
#' @param new.name name of output data frame (defaults to input data name)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
do_column_summary <- function(data_,
                              FUN = sum,
                              name,
                              modify,
                              ignore,
                              input,
                              output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Name of new column
  if (!hasArg(name)) {
    name <- deparse(substitute(FUN))
  }


  # Define columns to modify

  # ignore
  if (!hasArg(ignore)) {
    ignore <- c()
  }

  # modify
  if (!hasArg(modify)) {
    modify <- .data_columns(data_attributes)
  }

  modify <- setdiff(modify, ignore)


  # Summarize data
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(!!name := FUN(c_across(cols = dplyr::all_of(modify))),
                  .after = 1) %>%
    dplyr::ungroup()

  # Reset attributes
  data <- data %>%
    .set_data_attributes(data_attributes) %>%
    .add_data_attributes(which = paste0(data_attributes[["rows"]], "_data"),
                         new_attr = name)


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
