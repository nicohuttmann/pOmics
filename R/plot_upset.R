#' Visualizes identification data as UpSet plot
#'
#' @param data_ data_ list
#' @param order order of samples
#' @param nintersects number of bars representing intersects
#' @param transpose transpose data frame
#' @param from.row.names row names column
#' @param view view plot
#' @param input name of input data
#' @param output name of output plot
#'
#' @return
#' @export
#'
#'
plot_upset <- function(data_,
                       order,
                       nintersects = 10,
                       transpose,
                       from.row.names,
                       view = T,
                       input,
                       output = "plot_upset") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used
    
  }


  # Transpose data frame
  if (!hasArg(transpose)) {
    if ("observations" %in% colnames(data)) {
      transpose <- TRUE
      from.row.names <- "observations"
    } else {
      transpose <- FALSE
    }
  }

  if (transpose) {
    data <- transpose_tibble(tibble = data,
                             from.row.names = from.row.names)
  }


  # Transform dataframe to list
  data.list <- data2list(data)


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
  if (input_list[["list.input"]]) data_[[output]] <- p

  else data_ <- p

  # Return
  return(invisible(data_))

}
