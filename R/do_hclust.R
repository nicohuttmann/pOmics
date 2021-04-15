#' Performs hierarchical clustering on data frame
#'
#' @param data data
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data) {


  data <- get_data(data.name = "LFQ.imp", dataset = 2) %>%
    include_groups(groups = groups) %>%
    scale_()


  dendro.observations <- data %>%
    dplyr::select(where(is.numeric)) %>%
    dist() %>%
    hclust()

   plot(dendro.observations)


  # Variables
  dendro.variables <- hclust(d = dist(x = t(data)), method = clustering.method)


}
