#' Performs hierarchical clustering on data frame
#'
#' @param data_ data list
#' @param data.name name of data to use for analysis
#' @param scale Scale data (Z-score)?
#' @param group.column column to use for combining groups
#' @param grouping.function function to combine groups
#' @param distance.method method to calculate distance; see ?dist for options
#' @param clustering.method method to cluster columns and rows; see ?hclust for options
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data_, data.name = "raw_data", scale = T,
                      group.column = "groups", grouping.function = mean,
                      distance.method = "euclidean", clustering.method = "complete") {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!data.name %in% names(data_)) stop("Data could not be found. Please specify correct <data.name>.")


  # Get data
  data <- data_[[data.name]]

  # Scale data
  if (scale) data <- scale_(data)

  # Combine groups
  data <- collapse_groups(data = data, FUN = grouping.function, group.column = group.column)

  # Save data
  data_[["data"]] <- data

  # Dendrogram for x-axis (observations mostly)
  data_[["dend_x"]] <- data %>%
    dplyr::select(c(!!group.column, where(is.numeric))) %>%
    tibble2matrix(row.names = group.column) %>%
    dist(method = distance.method) %>%
    hclust(method = clustering.method)



  # Dendrogram for x-axis (variables mostly)
   data_[["dend_y"]] <- data %>%
     dplyr::select(c(!!group.column, where(is.numeric))) %>%
     tibble2matrix(row.names = group.column) %>%
     t() %>%
     dist(method = distance.method) %>%
     hclust(method = clustering.method)


   # Plot
   data_ <- plot_gg_heatmap(data_ = data_, export = F)

   # Return
   invisible(data_)

}
