#' Returns default data
#'
#' @param data.origin data origin program
#' @param type info type
#' @param silent suppress messages
#'
#' @return
#' @export
#'
#'
get_defaults <- function(data.origin, type, silent = F) {

  # Check default data
  new_default_data()


  # Check if data origin is known
  if (hasArg(data.origin) && !hasArg(type)) return(ifelse(data.origin %in% names(.info[["defaults"]]), TRUE, FALSE))

  # Set data.origin to NA if not known
  if (!hasArg(data.origin)) data.origin <- "na"


  # Return all defaults if no type is given
  if (!hasArg(type)) {
    # Print all defaults and stop
    if (!silent) {
      print(.info[["defaults"]][[data.origin]])
      message("No default type given.")
    }
    return(NA)
  }


  # Check type
  if (!type %in% names(.info[["defaults"]][[data.origin]])) {
    # Print all defaults and stop
    if (!silent) {
      print(.info[["defaults"]][[data.origin]])
      message("Default type not found.")
    }
    return(NA)
  }

  # Return
  return(.info[["defaults"]][[data.origin]][[type]])

}
