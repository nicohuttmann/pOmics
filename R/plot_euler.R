#' Plots Euler diagram from dataframe
#'
#' @param data_ data_ list
#' @param transpose transpose data frame
#' @param from.row.names row names column of initial data frame
#' @param to.row.names row names column after transposing (must be variables)
#' @param view view plot
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_euler <- function(data_,
                       fontsize = 8,
                       transpose = T,
                       from.row.names = "observations",
                       to.row.names = "variables",
                       view = T,
                       input,
                       output = "plot_euler") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
  }



  # Transpose data frame
  if (transpose) {

    data <- transpose_tibble(tibble = data,
                             from.row.names = from.row.names,
                             to.row.names = to.row.names)

  }


  # Check data
  if (typeof(data[[1]]) != "character" ||
      any(unlist(lapply(data[-1], typeof)) != "logical")) {

    message("Dataframe must consist of one character and n logical columns.")
    return(invisible(data_))

  }


  # Make plot
  p <- data %>%
    dplyr::select(-variables) %>%
    eulerr::euler() %>%
    plot(fill = "transparent",
         labels = list(fontsize = fontsize),
         quantities = list(fontsize = fontsize))


  # View plot
  if (view) print(p)



  # Prepare return
  if (input_list[["list.input"]]) data_[[output]] <- p

  else data_ <- p

  # Return
  return(invisible(data_))

}
