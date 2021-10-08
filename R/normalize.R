#' Normalizes and returns data
#'
#' @param data data
#' @param method method to use (default = "pqn", "none")
#'
#' @return
#' @export
#'
#'
normalize <- function(data, method = "pqn") {

  #
  if (method == "none") return(data)

  #
  else if (method == "pqn") return(pqn(data))

  #
  else stop("Normalization method not found.")

}
