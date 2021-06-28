#' Visualizes identification data as UpSet plot
#'
#' @param data_ data_ list
#' @param order order of samples
#' @param nintersects number of bars representing intersects
#' @param transpose transpose data frame
#' @param from.row.names row names column
#' @param view view plot
#' @param input input data
#' @param output output plot name
#'
#' @return
#' @export
#'
#'
plot_upset <- function(data_, order, nintersects = 10, transpose = T, from.row.names = "observations",
                       view = T, input = "Peptides", output = "plot_upset") {

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    return(invisible(NULL))

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    return(invisible(data_))

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_


  # Transpose data frame
  if (transpose) {

    data <- transpose_tibble(tibble = data,
                           from.row.names = from.row.names,
                           to.row.names = "variables")

  }


  # Transform dataframe to list
  data.list <- logical_df2list(data)


  # Make plot
  if (hasArg(order)) {
    p <- UpSetR::upset(UpSetR::fromList(data.list),
                          order.by = "freq",
                          sets = rev(order),
                          nsets = length(data.list),
                          keep.order = T,
                       nintersects = nintersects)
  } else {
    p <- UpSetR::upset(UpSetR::fromList(data.list),
                          order.by = "freq",
                          nsets = length(data.list),
                          keep.order = T,
                       nintersects = nintersects)
  }



  # View plot
  print(p)


  # Prepare return
  if (list.input) data_[[output]] <- p

  else data_ <- p

  # Return
  return(invisible(data_))

}
