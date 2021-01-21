#' Returns default data
#'
#' @param data.origin data origin program
#' @param type info type
#'
#' @return
#' @export
#'
#'
get_defaults <- function(data.origin, type) {

  # Check default data
  new_default_data()


  # Set data.origin to NA if not known
  if (!hasArg(data.origin) || !data.origin %in% names(.info[["defaults"]])) data.origin <- "na"


  # Return all defaults if no type is given
  if (!hasArg(type)) {
    # Print all defaults and stop
    print(.info[["defaults"]][[data.origin]])
    stop("No default type given.")
  }


  # Check type
  if (!type %in% names(.info[["defaults"]][[1]])) {
    # Print all defaults and stop
    print(.info[["defaults"]][[data.origin]])
    stop("Default type not found.")
  }

  # Return
  return(.info[["defaults"]][[data.origin]][[type]])

}
