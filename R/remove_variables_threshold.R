#' Removes columns from data frame based on minimum values above 0
#'
#' @param data data
#' @param min min. fraction of values above 0
#'
#' @return
#' @export
#'
#'
remove_variables_threshold <- function(data, min = 0.5) {


  if (is.matrix(data)) {

    data <- data[, apply(X = data,
                         MARGIN = 2,
                         FUN = function(x) mean(x > 0) >= min)]
  } else if (tibble::is_tibble(data)) {


    data <- dplyr::select(data, -where(function(x) is.numeric(x) & mean(x > 0) < min))


  } else {
    stop("Only matrices and tibbles supported")
  }

  # Return
  return(data)

}
