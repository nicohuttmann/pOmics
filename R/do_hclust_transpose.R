#' Changes x and y dendrograms and transposes hclust data
#'
#' @param data_ list or tibble
#' @param from.row.names row names column from initial data
#' @param to.row.names row names column after transposing
#' @param dataset dataset
#' @param input name of input data (must be "data_hclust)
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_hclust_transpose <- function(data_,
                                from.row.names,
                                to.row.names,
                                dataset,
                                input = "data_hclust",
                                output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
  }



  # Transpose data and change dendrograms
  data[["data"]] <- do_transpose(data[["data"]],
                                   from.row.names = from.row.names,
                                   to.row.names = to.row.names)

  dend_x <- data[["dend_y"]]

  data[["dend_y"]] <- data[["dend_x"]]

  data[["dend_x"]] <- dend_x

  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
