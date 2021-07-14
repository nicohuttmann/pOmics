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
collapse_rows <- function(data_, FUN = mean, by = "All", input, output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }



  if (by == "All" & !(by %in% names(data))) data <- dplyr::mutate(data, All = T)

  if (!by %in% names(data)) {

    message("Group column not found in data frame.")
    return(invisible(data_))

  }

  # Add new data frame
  data <- data %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = where(is.numeric) | where(is.logical), .fns = FUN))


  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
