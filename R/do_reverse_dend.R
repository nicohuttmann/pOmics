#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_reverse_dend <- function(data_,
                            which = "y",
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



  # Ensure only x and y can be modified
  which <- intersect(c("x", "y"), which)

  # Reverse hclust objects
  for (i in which) {

    data[[paste0("dend_", i)]] <- data[[paste0("dend_", i)]] %>%
      as.dendrogram() %>%
      rev() %>%
      as.hclust()

  }



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
