#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param dend which dendrogram to use for analysis
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_dend_enrich <- function(data_,
                           dend = "dend_y",
                           ...,
                           dataset,
                           input = "data_cormat",
                           output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
  }



  data <- data











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
