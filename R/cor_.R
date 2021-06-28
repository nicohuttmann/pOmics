#' Calculates correlation matrix and return cor_list
#'
#' @param data LFQ data
#' @param method method to calculate correlation
#' @param set.diag.0 set diagonal to zero
#'
#' @return
#' @export
#'
#'
cor_ <- function(data, method = "pearson", set.diag.0 = F) {

  # Setup a list to store all correlation data
  cor_list <- tibble::lst()

  # Add raw data
  cor_list[["raw_data"]] <- data

  # Add variable names
  attr(cor_list, "variables") <- colnames(data)

  # Add correlation matrix
  cor_list[["correlation"]] <- cor(x = data, method = method)

  # Set diagonal to zero
  if (set.diag.0) diag(cor_list[["correlation"]]) <- 0

  # Add attributes
  attr(cor_list, "correlation") <- method

  # Return
  return(invisible(cor_list))

}
