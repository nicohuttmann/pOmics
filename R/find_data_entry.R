#' Identifies data entrys by column names
#'
#' @param data rawdata list
#' @param entry entry type
#' @param data.origin (optional) data origin
#'
#' @return
#' @export
#'
#'
find_data_entry <- function(data, entry, data.origin) {

  # Column/names of rawdata
  names = names(data)[unlist(lapply(data, function(x) !is.matrix(x) && is.character(x)))]


  column <- get_defaults(data.origin = data.origin,
                         type = paste0("column_", entry))


  # If column found by default
  if (column %in% names) {
    return(column)
    # "NA" string indicates column cannot be found
  } else if (column == "NA") {
    return(NA)
    # Search by pattern
  } else {
    return(findORchoose(names = names,
                        patterns = get_defaults(data.origin,
                                                paste0("pattern_", entry)),
                        title = get_defaults(data.origin,
                                             paste0("question_", entry))))
  }

}
