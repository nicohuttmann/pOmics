#' Plots Euler diagram from dataframe
#'
#' @param data dataframe
#' @param view view plot
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
plot_euler <- function(data, view = T) {

  # Check data
  if (typeof(data[[1]]) != "character" || any(unlist(lapply(data[-1], typeof)) != "logical"))
    stop("Dataframe must consist of one character and n logical columns.")


  # Make plot
  plot <- data %>%
    dplyr::select(-variables) %>%
    eulerr::euler() %>%
    plot(quantities = TRUE)


  # View plot
  if (view) print(plot)

  # Return
  invisible(plot)

}
