#' Calculates adjacency matrix from similarity matrix
#'
#' @param data similarity matrix
#' @param method adjacency function ("sigmoid", "power", "none")
#' @param alpha alpha parameter for sigmoid function
#' @param theta theta parameter for sigmoid function
#' @param beta beta parameter for power function
#'
#' @return
#' @export
#'
#'
adjacency <- function(data, method, alpha, theta, beta) {

  #
  if (!method %in% c("sigmoid", "power", "none"))
  stop("Adjacency function not known. Use sigmoid, power or none.")

  # Set diagonal to 0
  data[col(data) == row(data)] <- 0


  # Sigmoid function
  if (method == "sigmoid" && hasArg(alpha) && hasArg(theta)) {

    data <- 1 / (1 + exp(- alpha * (data - theta)))

    # Power function
  } else if(method == "power") {

    data <- data ^ beta

    # could not be computed
  } else if(method == "none") {

    data <- data

  } else {
    stop("Wrong AF name or parameter missing.")
  }

  #
  return(data)

}
