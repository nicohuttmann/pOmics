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
  names <- names(data)[unlist(lapply(data, function(x) !is.matrix(x) && is.character(x)))]


  default <- get_defaults(data.origin = data.origin,
                         type = entry)




  # If column found by default
  if (default[["column"]] %in% names) {
    return(default[["column"]])
    # "NA" string indicates column cannot be found
  } else if (default[["column"]] == "NA") {
    return(NA)
    # Search by pattern
  } else {
    return(findORchoose(names = names,
                        patterns = default[["pattern"]],
                        title = default[["question"]]))
  }

}
