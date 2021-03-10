#' Same as strsplit but returns vector
#'
#' @param string string
#' @param split split character
#'
#' @return
#' @export
#'
#'
strsplit_ <- function(string, split = ";") {
  
  # Return splitted string as vector
  return(unlist(strsplit(string, split = split)))
  
}
