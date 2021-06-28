#' Plots data as bar plot with ggplot2
#'
#' @param data data to plot
#' @param x column name of x axis labels
#' @param y numeric value to be represented
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
plot_gg_bar <- function(data, x = "observations", y = "count") {

  # Check input
  if (!hasArg(data)) stop("No data frame given.")

  # Check input type
  if (!is.data.frame(data)) stop("Given data is not a data frame.")

  # Check data list
  if (!x %in% names(data) | !y %in% names(data)) stop("Data column can not be found. Please specify correct <x> and <y>.")


  # Plot
  p <- ggplot(data = data, mapping = aes_string(x = x, y = y)) +
    geom_bar(stat = "identity") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))

  # Print plot
  print(p)


  # Return
  return(invisible(p))

}
