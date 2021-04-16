#' Performs hierarchical clustering on data frame
#'
#' @param data_
#' @param scale Scale data (Z-score)?
#' @param group.column
#' @param grouping.function
#' @param clustering.method
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data_, scale = T, group.column = "groups", grouping.function = mean, clustering.method) {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!data.name %in% names(data_)) stop("Data could not be found. Please specify correct 'data.name'.")


  # Get data
  data <- data_[[data.name]]

  # Scale data
  if (scale) data <- scale_(data)



  set_default_dataset("Iron")

  data <- get_data(data.name = "LFQ.imp", observations = All, dataset = ) %>%
    include_groups(groups = group.column, dataset = ) %>%
    scale_()


  data <- collapse_groups(data = data, FUN = grouping.function, group.column = group.column)


  dendro.observations <- data %>%
    dplyr::select(c(!!group.column, where(is.numeric))) %>%
    tibble2matrix(row.names = group.column) %>%
    dist(method = "euclidean") %>%
    hclust(method = "complete")

   plot(dendro.observations)


  # Variables
  dendro.variables <- hclust(d = dist(x = t(data)), method = clustering.method)


}
