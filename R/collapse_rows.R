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
    input <- input_list[["input"]] # Remove if not used

  }

  # Add dummy variable for all
  if (by == "All" & !(by %in% names(data)))
    data <- dplyr::mutate(data, All = "all")

  if (!by %in% names(data)) {

    message("Group column not found in data frame.")
    return(invisible(data_))

  }

  # Keep initial data frame for later
  data0 <- data

  # Add new data frame
  data <- data %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = where(is.numeric) | where(is.logical),
                            .fns = FUN))

  # Collapse character and
  data0 <- data0 %>%
    dplyr::group_by(!!dplyr::sym(by)) %>%
    dplyr::summarise(across(.cols = where(is.character) | where(is.factor),
                            .fns = function(x) if (length(unique(x)) == 1) x[1]
                                               else NA)) %>%
    dplyr::select(where(~!any(is.na(.))))

  # Combine data frames
  data <- dplyr::full_join(data0, data, by = by)

  # Rename grouping column to observations
  data <- data %>%
    dplyr::rename("observations" := !!by)

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
