#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param buttons (optional) add buttons ("copy", "csv", "excel", "pdf",
#' "print")
#' @param dom order of table elements (default: "lBfrtip", see
#' https://rstudio.github.io/DT/)
#' @param view view table
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
export_RMD_fun_enrich <- function(data_,
                                  buttons,
                                  dom = "lBfrtip",
                                  view = F,
                                  input = "fun_enrich",
                                  output = "fun_enrich_DT") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  # Prepare all sub data frames
  for (i in seq_along(data)) {

    for (j in seq_along(data[[i]])) {

      data[[i]][[j]] <- export_DT_fun_enrich(data[[i]][[j]],
                                             buttons = buttons,
                                             dom = dom)

    }

  }


  if (view) print(data)

  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}

