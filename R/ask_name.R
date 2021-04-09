#' Asks user for name if no name given
#'
#' @param name name
#' @param message message
#' @param exclude exclusion list for unavailable names
#'
#' @return
#' @export
#'
#'
ask_name <- function(name, message, exclude = NA) {

  #
  if (hasArg(name) && !name %in% exclude) {
    return(name)
  } else {
    name <- ""
    while(name == "" || name %in% exclude) name <- readline(ifelse(hasArg(message), message, "Name: "))
    return(name)
  }

}
