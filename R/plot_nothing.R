#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param ... specific arguments
#' @param dataset dataset
#' @param view print plot
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
plot_nothing <- function(data_,
                         ...,
                         dataset,
                         view = T,
                         export = F,
                         file = "nothing.pdf",
                         height = 3,
                         input = "data_nothing",
                         output = "plot_nothing") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }



  data <- data


  # Print plot
  if (view) print(p)

  # Export plot
  if (export) {

    export_pdf(p, file = file,
               width,
               heigth,
               open = F)

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
