#' Collapses groups of data frame by specified function and adds as list entry
#'
#' @param data_ data
#' @param FUN function to combine groups
#' @param group.column column specifying the groups
#' @param data.name name of data frame entry
#' @param new.name new entry name
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
collapse_groups_ <- function(data_, FUN = mean, group.column = "groups", data.name = "raw_data", new.name = "grouped_data") {

  # Check input
  if (!hasArg(data_) | !is.list(data_) | !tibble::is_tibble(data_[[data.name]])) stop("Please provide a list with the respective tibble.")

  if (!group.column %in% names(data_[[data.name]])) stop("Group column not found in data frame.")

  # Add new data frame
  data_[[new.name]] <- data_[[data.name]] %>%
    dplyr::group_by(!!dplyr::sym(group.column)) %>%
    dplyr::summarise(across(.cols = where(is.numeric) | where(is.logical), .fns = FUN))

  # Return
  return(data_)

}
