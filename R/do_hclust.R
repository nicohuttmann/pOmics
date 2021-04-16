#' Performs hierarchical clustering on data frame
#'
#' @param data data
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data, clustering.method) {


  data <- get_data(data.name = "LFQ.imp", observations = , dataset = ) %>%
    include_groups(groups = groups, dataset = ) %>%
    scale_()


  dendro.observations <- data %>%
    dplyr::select(where(is.numeric)) %>%
    dist() %>%
    hclust()

   plot(dendro.observations)


  # Variables
  dendro.variables <- hclust(d = dist(x = t(data)), method = clustering.method)


}
