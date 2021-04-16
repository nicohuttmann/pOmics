#' Performs hierarchical clustering on data frame
#'
#' @param data data
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data_, group.column = "groups", grouping.function = mean, clustering.method) {

  set_default_dataset("DEN")

  data <- get_data(data.name = "LFQ.imp", observations = clean, dataset = ) %>%
    include_groups(groups = groups, dataset = ) %>%
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
