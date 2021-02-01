#' Checks if .info data exists
#'
#' @param name name of .info data
#'
#' @return
#' @export
#'
#'
is_info_data <- function(name) {
  
  # Check input
  if (!hasArg(name)) stop("No name given.")
  
  # Check .info names
  if (name %in% names(.info)) return(TRUE)
  else return(FALSE)
  
}
