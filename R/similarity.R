#' calculates similarity from correlation
#'
#' @param data correlation matrix
#' @param method similarity function
#'
#' @return
#' @export
#'
#'
similarity <- function(data, method) {

  #
  if (!method %in% c("absolute", "preserve", "none")) stop("Similarity function not known.")

  #
  if (method == "absolute") data <- abs(data)

  if (method == "preserve") data <- (1 + data) / 2

  if (method == "none") data <- data

  #
  return(data)


}
