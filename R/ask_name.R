#' Asks user for name if no name given
#'
#' @param name name
#' @param message message
#'
#' @return
#' @export
#'
#'
ask_name <- function(name, message) {

  #
  if (hasArg(name)) {
    return(name)
  } else {
    name <- ""
    while(name == "") name <- readline(ifelse(hasArg(message), message, "Name: "))
    return(name)
  }

}
