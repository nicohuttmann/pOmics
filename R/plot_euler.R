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
plot_euler <- function(data_, fontsize = 8, transpose = T, from.row.names = "observations", to.row.names = "variables", view = T,
                       input = "Peptides", output = "plot_euler") {

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    invisible(NULL)

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    invisible(data_)

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_



  # Transpose data frame
  if (transpose) {

    data <- transpose_tibble(tibble = data,
                             from.row.names = from.row.names,
                             to.row.names = to.row.names)

  }


  # Check data
  if (typeof(data[[1]]) != "character" || any(unlist(lapply(data[-1], typeof)) != "logical")) {

    message("Dataframe must consist of one character and n logical columns.")
    invisible(data_)

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
  if (list.input) data_[[output]] <- p

  else data_ <- p

  # Return
  invisible(data_)

}
