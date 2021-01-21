#' Checks if given data.origin is among the defaults
#'
#' @param data.origin data origin
#'
#' @return
#' @export
#'
#'
is_data_origin <- function(data.origin) {

  # Check default data
  new_default_data()

  #
  if (!hasArg(data.origin) || !data.origin %in% names(.info[["defaults"]])) return(FALSE)

  #
  else return(TRUE)



}
