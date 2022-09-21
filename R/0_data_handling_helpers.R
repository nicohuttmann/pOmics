#' Handles list input for functions
#'
#' @param data_ data_
#' @param input name of input data
#'
#' @export
#'
#'
data_input <- function(data_, input) {

  # List to return
  input_list <- list(data = NULL,
                     input = "input",
                     list.input = TRUE,
                     error = FALSE)

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    input_list[["error"]] <- TRUE

    return(input_list)

  }


  # Check if list or data frame given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Input
  if (list.input) {

    if (!hasArg(input)) {

      input <- get_input(data_ = data_)

    }

    # Check list input
    if (list.input & !input %in% names(data_)) {

      message("Data could not be found. Please specify correct <input>.")

      input_list[["data"]] <- data_

      input_list[["error"]] <- TRUE

      return(invisible(input_list))

    }

    input_list[["data_attributes"]] <- .get_data_attributes(data_[[input]])

    input_list[["data"]] <- sticky::unsticky(data_[[input]])

    input_list[["input"]] <- input

  } else {

    input_list[["data_attributes"]] <- .get_data_attributes(data_)

    input_list[["data"]] <- sticky::unsticky(data_)

    input_list[["list.input"]] <- FALSE

  }

  # Return
  return(input_list)

}


#' Transforms tibble to matrix
#'
#' @param tibble tibble
#' @param from.row.names row to use as new row names
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
tibble2matrix <- function(tibble, from.row.names) {

  # Save data_attributes
  data_attributes <- .get_data_attributes(tibble)

  # Make data unsticky
  data <- sticky::unsticky(data)

  # Determine row.names
  if (!hasArg(from.row.names)) {

    if (!is.null(data_attributes)) {

      from.row.names <- data_attributes[["rows"]]

    } else {

      from.row.names <- colnames(tibble)[1]

    }

  }

  # Determine columns to keep
  if (!is.null(data_attributes)) {
    keep <- c(from.row.names, .data_columns(data_attributes))
  } else {

    # Make sure data type does not change
    class.k <- tibble %>%
      dplyr::select(-dplyr::any_of(from.row.names)) %>%
      sapply(class) %>%
      table() %>%
      sort(decreasing = T) %>%
      names() %>%
      first_element()

    keep <- c(from.row.names,
              colnames_class(data = tibble, data.class = class.k))

  }


  # Transform to tibble
  data <- tibble %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    tibble::column_to_rownames(var = from.row.names) %>%
    as.matrix()

  # Reset data_attributes
  data <- .set_data_attributes(data, data_attributes)


  # Transform and return
  return(data)

}


#' Transforms matrix to tibble and adds column for row names
#'
#' @param data matrix with row names
#' @param to.row.names name for row names vector
#'
#' @export
#'
#'
matrix2tibble <- function(data, to.row.names) {

  # Save data_attributes
  data_attributes <- .get_data_attributes(data)

  # Make data unsticky
  data <- sticky::unsticky(data)

  # Determine row.names
  if (!hasArg(to.row.names)) {

    if (!is.null(data_attributes)) {

      to.row.names <- data_attributes[["rows"]]

    } else {

      # row.names <- "observations"
      if (nrow(data) < ncol(data)) row.names <- "observations"

      else row.names <- "variables"

    }

  }


  # Transform to tibble
  data <- data %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = to.row.names) %>%
    tibble::as_tibble()

  # Reset data_attributes
  data <- .set_data_attributes(data, data_attributes)

  # Transform and return
  return(data)

}


#' Transposes tibble and uses first column as column names
#'
#' @param tibble tibble
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
transpose_tibble <- function(tibble,
                             from.row.names,
                             to.row.names) {

  # Save data_attributes
  data_attributes <- .get_data_attributes(tibble)

  # Make data unsticky
  tibble <- sticky::unsticky(tibble)


  # No defined data_attributes
  if (is.null(data_attributes)) {

    if (!hasArg(from.row.names)) from.row.names <- colnames(tibble)[1]

    if (!hasArg(to.row.names)) {

      if (from.row.names == "variables") to.row.names <- "observations"

      else if (from.row.names == "observations") to.row.names <- "variables"

      else to.row.names <- "rows"
    }

    # Define columns to keep
    data.class.1 <- tibble %>%
      sapply(class) %>%
      table() %>%
      sort(decreasing = TRUE) %>%
      names() %>%
      first_element()

    keep <- c(from.row.names,
              colnames_class(tibble, data.class.1))

    # data_attributes exist
  } else {

    from.row.names <- data_attributes[["rows"]]

    to.row.names <- data_attributes[["columns"]]

    keep <- c(from.row.names, .data_columns(data_attributes))

  }


  # Transpose
  tibble.t <- tibble %>%
    dplyr::select(dplyr::all_of(keep)) %>%
    tibble2matrix(from.row.names = from.row.names) %>%
    t() %>%
    matrix2tibble(to.row.names = to.row.names)


  # Reset data_attributes
  tibble.t <- .set_data_attributes(
      data = tibble.t,
      data_attributes = .transpose_data_attributes(data_attributes))


  # Return transposed tibble
  return(tibble.t)

}


#' Transforms data frames to tibble and adds column for row names
#'
#' @param data data frame with row names
#' @param to.row.names name for row names vector
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
data.frame2tibble <- function(data, to.row.names) {

  # Save data_attributes
  data_attributes <- .get_data_attributes(data)

  # Make data unsticky
  data <- sticky::unsticky(data)


  # Determine row.names
  if (!hasArg(to.row.names)) {

    if (!is.null(data_attributes)) {

      to.row.names <- data_attributes[["rows"]]

    } else {

      # row.names <- "observations"
      if (nrow(data) < ncol(data)) row.names <- "observations"

      else row.names <- "variables"

    }

  }


  # Transform data.frame
  data <- data %>%
    tibble::rownames_to_column(var = to.row.names) %>%
    tibble::as_tibble()

  # Reset data_attributes
  data <- .set_data_attributes(data, data_attributes)


  # Transform and return
  return(data)

}


#' Transforms tibble to data frame
#'
#' @param tibble tibble
#' @param from.row.names column to use as new row names
#'
#' @export
#'
tibble2data.frame <- function(tibble, from.row.names) {

  # Save data_attributes
  data_attributes <- .get_data_attributes(tibble)

  # Make data unsticky
  tibble <- sticky::unsticky(tibble)

  # Determine row.names
  if (!hasArg(from.row.names)) {

    if (!is.null(data_attributes)) {

      from.row.names <- data_attributes[["rows"]]

    } else {

      from.row.names <- colnames(tibble)[1]

    }

  }


  # Transform to data frame
  data <-  tibble::column_to_rownames(tibble, var = from.row.names)

  # Reset data_attributes
  data <- .set_data_attributes(data, data_attributes)


  # Transform and return
  return(data)

}


#' Transforms any data type to a tibble
#'
#' @param data supported data types: matrix, data.frame
#' @param to.row.names name for row names vector
#'
#' @export
#'
#'
data2tibble <- function(data, to.row.names) {

  # Already a tibble
  if (tibble::is_tibble(data)) {
    return(data)

    # Matrix
  } else if (is.matrix(data)) {

    data <- matrix2tibble(data, to.row.names)

    # Data frame
  } else if (is.data.frame(data)) {

    data <- data.frame2tibble(data, to.row.names)

    # Other data types
  } else {
    stop("Data type not supported yet. Please contact Nico.")
  }

  # Return
  return(data)

}


#' Transforms a list to tibble logical indication for variables
#'
#' @param x list
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
list2tibble <- function(x, identifier = "variables") {

  #
  df <- tibble::tibble(!!identifier := unique(unlist(x)))

  # Add logical columns
  for (column in names(x)) {
    df <- df %>%
      dplyr::mutate(!!column := variables %in% x[[column]])
  }

  # Return
  return(df)

}
