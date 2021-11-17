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
#' @importFrom magrittr %>%
#'
plot_ComplexHeatmap <- function(data_,
                                ...,
                                dataset,
                                view = T,
                                input = "data_hclust",
                                output = "plot_ComplexHeatmap") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used
  }


  # ---- Store arguments in list ----
  input.args <- list(...)


  # ---- List to collect arguments for arguments for Heatmap() function ----
  heatmap.args <- list()


  # ---- Define arguments for ComplexHeatmap

  # ---- mat ----
  heatmap.args[["mat"]] <- data[["data"]] %>%
    tibble2matrix()

  # ---- cluster_rows ----
  # Test for given input (TODO: control input more)
  if (!is.null(input.args[["cluster_rows"]])) {
    heatmap.args[["cluster_rows"]] <- input.args[["cluster_rows"]]
  # Use hclust object from data_hclust
  } else {
    heatmap.args[["cluster_rows"]] <- data[["dend_y"]]
  }

  # ---- cluster_columns ----
  # Test for given input (TODO: control input more)
  if (!is.null(input.args[["cluster_columns"]])) {
    heatmap.args[["cluster_columns"]] <- input.args[["cluster_columns"]]
    # Use hclust object from data_hclust
  } else {
    heatmap.args[["cluster_columns"]] <- data[["dend_x"]]
  }


  # ---- Row names ----
  for (i in names(input.args)) {
    heatmap.args[[i]] <- input.args[[i]]
  }



  # Make ComplexHeatmap plot
  p <- do.call(ComplexHeatmap::Heatmap, heatmap.args)
  p

  # Print plot
  if (view) ComplexHeatmap::draw(p)


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
