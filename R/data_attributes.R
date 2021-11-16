#' Saves relevant data attributes in list
#'
#' @param data data object with
#'
#' @return
#' @export
#'
#'
.get_data_attributes <- function(data) {

  data_attributes <- attributes(data)

  data_attributes <- data_attributes[intersect(names(data_attributes),
                                               c("dataset",
                                                 "data",
                                                 "rows",
                                                 "columns",
                                                 "variables",
                                                 "observations",
                                                 "other",
                                                 "variables_data",
                                                 "observations_data",
                                                 "other_data"))]

  if (length(data_attributes) == 0) return(NULL)

  # Return
  return(data_attributes)

}


#' Sets relevant data attributes from list
#'
#' @param data data object with specific attributes
#' @param data_attributes list of data_attributes
#'
#' @return
#' @export
#'
#'
.set_data_attributes <- function(data, data_attributes, overwrite = T) {

  if(is.null(data_attributes)) return(data)


  existing_attributes <- .get_data_attributes(data)

  if (!overwrite)
    data_attributes <- data_attributes[setdiff(names(data_attributes),
                                               names(data_attributes))]

  # Ensure data is sticky
  data <- sticky::sticky(data)

  # Set attributes
  for (i in names(data_attributes)) {
    attr(data, which = i) <- data_attributes[[i]]
  }


  # Update data attributes
  data <- .update_data_attributes(data)

  # Return
  return(data)

}


#' Compares data attributes with data and removes non-existing entries
#'
#' @param data data object with specific attributes
#'
#' @return
#' @export
#'
#'
.update_data_attributes <- function(data) {

  # Get current data attributes
  data_attributes <- .get_data_attributes(data)

  # Update data rows
  if (!is.matrix(data)) {

    data_attributes[[attr(data, "rows")]] <-
      intersect(data_attributes[[attr(data, "rows")]],
                data[[attr(data, "rows")]])

  } else {

    data_attributes[[attr(data, "rows")]] <-
      intersect(data_attributes[[attr(data, "rows")]],
                row.names(data))

  }


  # Update data columns
  data_attributes[[attr(data, "columns")]] <-
    intersect(data_attributes[[attr(data, "columns")]],
              colnames(data))

  # Update meta data
  meta_data <- paste0(data_attributes[["rows"]], "_data")
  data_attributes[[meta_data]] <-
    intersect(data_attributes[[meta_data]], colnames(data))


  # Set attributes
  for (i in names(data_attributes)) {
    attr(data, which = i) <- data_attributes[[i]]
  }


  # Return
  return(data)

}


#' Adds entries to data_attributes
#'
#' @param data data object with specific attributes
#' @param which which attribute type
#' @param new_attr new attributes
#'
#' @return
#' @export
#'
#'
.add_data_attributes <- function(data, which, new_attr) {

  # Check input
  if (!hasArg(data)) {

    stop("No data given to .add_data_attributes.")

  }

  # Get current data attributes
  data_attributes <- .get_data_attributes(data)

  # Add attribute to list
  if (is.null(data_attributes[[which]])) {
    data_attributes[[which]] <- new_attr
  } else {
    data_attributes[[which]] <- c(data_attributes[[which]], new_attr)
  }


  # Add and update
  data <- .set_data_attributes(data, data_attributes)


  # Return
  return(data)

}


#' Get column names containing data independently if variables or data columns
#'
#' @param data_attributes data_attributes list
#'
#' @return
#' @export
#'
#'
.data_columns <- function(data_attributes) {

  # columns entry determines which attribute to use for column names
  data_cols <- data_attributes[[data_attributes[["columns"]]]]

  # Return
  return(data_cols)

}


#' Get column names containing data independently if variables or data columns
#'
#' @param data_attributes data_attributes list
#'
#' @return
#' @export
#'
#'
.transpose_data_attributes <- function(data_attributes) {

  if(is.null(data_attributes)) return(NULL)

  # Change rows and columns attributes
  rows <- data_attributes[["columns"]]
  columns <- data_attributes[["rows"]]

  data_attributes[["columns"]] <- columns
  data_attributes[["rows"]] <- rows

  # Return
  return(data_attributes)

}

