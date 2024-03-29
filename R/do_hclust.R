#' Performs hierarchical clustering on data frame
#'
#' @param data_ data list
#' @param scale Scale data (Z-score)?
#' @param distance.method method to calculate distance; see ?dist for options
#' @param clustering.method method to cluster columns and rows; see ?hclust for
#' options
#' @param plot generate default plot with plot_heatmap()
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data_,
                      scale = T,
                      distance.method = "euclidean",
                      clustering.method = "complete",
                      plot = T,
                      input,
                      output = "data_hclust") {

  # ---- Handle input ----
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  data <- input_list[["data"]]
  data_attributes <- input_list[["data_attributes"]]

  # Scale data
  if (scale) data <- do_scale(data)



  # Dendrogram for x-axis (observations mostly)
  dend_x <- data %>%
    dplyr::select(c(1, where(is.numeric))) %>%
    tibble2matrix() %>%
    t() %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)


  # Dendrogram for x-axis (variables mostly)
  dend_y <- data %>%
    dplyr::select(c(1, where(is.numeric))) %>%
    tibble2matrix() %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)


  # ---- Reorder data based on dendrograms (simplifies analysis later) ----

  data <- data %>%
    dplyr::relocate(c(setdiff(colnames(data), dend_x[["labels"]][dend_x[["order"]]]),
                      dend_x[["labels"]][dend_x[["order"]]])) %>%
    dplyr::arrange(match(.[[1]], dend_y[["labels"]][dend_y[["order"]]]))



  # Save data
  data_[[output]] <- list()

  data_[[output]][["data"]] <- data


  data_[[output]][["dend_x"]] <- data %>%
    dplyr::select(c(1, where(is.numeric))) %>%
    tibble2matrix() %>%
    t() %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)


  # Dendrogram for x-axis (variables mostly)
  data_[[output]][["dend_y"]] <- data %>%
    dplyr::select(c(1, where(is.numeric))) %>%
    tibble2matrix(from.row.names = colnames(.)[1]) %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)


  # Plot
  if (plot) data_ <- plot_heatmap(data_ = data_, export = F, input = output)

  # Return
  return(invisible(data_))

}

