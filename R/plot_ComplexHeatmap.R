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

  # Save modified argument names
  modified <- c()

  # ---- mat ----
  if (input %in% c("data_hclust", "data_cormat")) {
    heatmap.args[["mat"]] <- data[["data"]] %>%
    tibble2matrix()
  } else {
    heatmap.args[["mat"]] <- data %>%
      tibble2matrix()
  }

  r.names <- rownames(heatmap.args[["mat"]])
  c.names <- colnames(heatmap.args[["mat"]])

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

  # ---- Row labels ----
  if (!is.null(input.args[["row_labels"]])) {
    heatmap.args[["right_annotation"]] <-
      ComplexHeatmap::HeatmapAnnotation(
        which = "row",
        lables = ComplexHeatmap::anno_mark(
          at = match(names(input.args[["row_labels"]]), r.names),
          labels = input.args[["row_labels"]]))
  }

  # ---- Row annotations ----
  if (!is.null(input.args[["TERM2GENE"]])) {

    ha.list <- list(which = "row")

    ha.col.list <- list()

    for (i in colnames(input.args[["TERM2GENE"]])[-1]) {

      ann <- pull_data(input.args[["TERM2GENE"]], i)

      ha.list[[i]] <- ann[r.names]

      ha.col.list[[i]] <- c("FALSE" = "white", "TRUE" = "black")

      ha.list[["col"]] <- ha.col.list

    }

    heatmap.args[["right_annotation"]] <-
      do.call(ComplexHeatmap::HeatmapAnnotation,
              ha.list)

    modified <- c(modified, "TERM2GENE")

  }

  # ---- Row split ----
  if (!is.null(input.args[["row_split"]])) {
    heatmap.args[["row_split"]] <- input.args[["row_split"]][r.names]

    modified <- c(modified, "row_split")
  }



  # ---- Transfer input ----
  for (i in setdiff(names(input.args), modified)) {
    heatmap.args[[i]] <- input.args[[i]]
  }



  # Make ComplexHeatmap plot
  p <- do.call(ComplexHeatmap::Heatmap, heatmap.args)


  # Print plot
  if (view) ComplexHeatmap::draw(p)


  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- p
    attr(data_, "data") <- output
  }

  else data_ <- p

  # Return
  return(invisible(data_))

}

