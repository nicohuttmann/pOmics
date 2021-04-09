#' Performs hierarchical clustering on data frame
#'
#' @param data data
#'
#' @return
#' @export
#'
#'
do_hclust <- function(data) {


  hc <- get_data(observations = All) %>%
    include_groups(groups = groups) %>%
    scale_()


  dendro.observations <- hc %>%
    dplyr::select(where(is.numeric)) %>%
    dist() %>%
    hclust()

   plot(dendro.observations)


  # Variables
  dendro.variables <- hclust(d = dist(x = t(data)), method = clustering.method)


}
