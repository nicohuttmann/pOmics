#' Returns possible input names
#'
#' @param data_ data_
#'
#' @return
#' @export
#'
#'
get_input <- function(data_) {

  # Check input
  if (!hasArg(data_) | (length(data_) < 1)) {

    message("No data given.")

    return(invisible(NULL))

  }

  # Dataset
  #dataset <- get_dataset(dataset = dataset, try.all = FALSE)

  # Determine input name
  if (is.null(attr(data_, "data"))) input.name <- rev(names(data_))[1]

  else input.name <- attr(data_, "data")

  # Return
  return(input.name)

}
