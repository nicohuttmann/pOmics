#' Performs hierarchical clustering on data frame
#'
#' @param data_ data list
#' @param scale Scale data (Z-score)?
#' @param distance.method method to calculate distance; see ?dist for options
#' @param clustering.method method to cluster columns and rows; see ?hclust for options
#' @param plot generate default plot with plot_gg_heatmap()
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data_, scale = T, distance.method = "euclidean", clustering.method = "complete", plot = T,
                      input = "LFQ.intensity", output = "data_hclust") {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!input %in% names(data_)) stop("Data could not be found. Please specify correct <input>.")


  # Get data
  data <- data_[[input]]

  # Scale data
  if (scale) data <- do_scale(data)




  # Dendrogram for x-axis (observations mostly)
  dend_x <- data %>%
    dplyr::select(c(colnames(.)[1], where(is.numeric))) %>%
    tibble2matrix(row.names = colnames(.)[1]) %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)

  # Dendrogram for x-axis (variables mostly)
  dend_y <- data %>%
    dplyr::select(c(colnames(.)[1], where(is.numeric))) %>%
    tibble2matrix(row.names = colnames(.)[1]) %>%
    t() %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)



   # Save data
   data_[[output]] <- data %>%
     dplyr::arrange(match(!!dplyr::sym(colnames(.)[1]), dend_x[["labels"]][dend_x[["order"]]])) #%>%
     #dplyr::select(c(colnames(.)[1], dend_y[["labels"]][dend_y[["order"]]]))




   data_[["dend_x"]] <- data_[[output]] %>%
     dplyr::select(c(colnames(.)[1], where(is.numeric))) %>%
     tibble2matrix(row.names = colnames(.)[1]) %>%
     dist(method = distance.method) %>%
     hclust(method = clustering.method)

   # Dendrogram for x-axis (variables mostly)
   data_[["dend_y"]] <- data_[[output]] %>%
     dplyr::select(c(colnames(.)[1], where(is.numeric))) %>%
     tibble2matrix(row.names = colnames(.)[1]) %>%
     t() %>%
     dist(method = distance.method) %>%
     hclust(method = clustering.method)



   # Plot
   if (plot) data_ <- plot_gg_heatmap(data_ = data_, export = F, input = output)

   # Return
   return(invisible(data_))

}
