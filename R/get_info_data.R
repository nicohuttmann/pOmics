#' Returns .info data
#'
#' @param name name of .info data
#'
#' @return
#' @export
#'
#'
get_info_data <- function(name) {
  
  # check if exists
  if (is_info_data(name)) {
    return(.info[[name]])
  } else {
    stop("Does not exist.")
  }
  
}
