#' Collapses groups of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups
#' @param group.column column specifying the groups
#' @param input name of input data frame
#' @param new.name name of output data frame (defaults to input data name)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
collapse_groups <- function(data_, FUN = mean, group.column = "groups", input = "Peptides", output) {

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



  if (!group.column %in% names(data)) {

    message("Group column not found in data frame.")
    return(invisible(data_))

  }

  # Add new data frame
  data <- data %>%
    dplyr::group_by(!!dplyr::sym(group.column)) %>%
    dplyr::summarise(across(.cols = where(is.numeric) | where(is.logical), .fns = FUN))


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
