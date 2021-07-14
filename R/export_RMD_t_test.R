#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param order.by order rows by given column
#' @param descending arrange in descending order
#' @param view view table
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
export_RMD_t.test <- function(data_, order.by = "p.adjust", descending = F, dataset, view = F, input = "data_t.test", output = "data_DT_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  #
  data <- export_DT_t.test(data = data, order.by = order.by, descending = descending)

  if (view) print(data)

  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data

  else data_ <- data

  # Return
  return(data_)

}
