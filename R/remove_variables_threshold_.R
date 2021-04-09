#' Removes columns from data frame based on minimum values above 0
#'
#' @param analysis_list analysis list
#' @param min min. fraction of values above 0
#' @param target.data data frame to be modified
#'
#' @return
#' @export
#'
#'
remove_variables_threshold_ <- function(analysis_list, min = 0.5, target.data = "raw_data") {

  # Check input
  if (!hasArg(analysis_list) |
      !(is.list(analysis_list)) |
      is.data.frame(analysis_list) |
      tibble::is_tibble(analysis_list) |
      is.matrix(analysis_list) |
      !target.data %in% names(analysis_list))
    stop("Please provide a list containg your indicated data as input.")

  # Extract data frame
  data <- analysis_list[[target.data]]

  # Apply threshold
  if (is.matrix(data)) {

    data <- data[, apply(X = data,
                         MARGIN = 2,
                         FUN = function(x) mean(x > 0) >= min)]
  } else if (tibble::is_tibble(data)) {


    data <- dplyr::select(data, -where(function(x) is.numeric(x) & mean(x > 0) < min))


  } else {
    stop("Only matrices and tibbles supported")
  }

  # Readd modified data
  analysis_list[[target.data]] <- data

  # Return
  return(analysis_list)

}
