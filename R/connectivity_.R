#'  Add connectivity and intracluster connectivity to cor_list
#'
#' @param cor_list cor_list object
#' @param k max number of clusters
#' @param scale Scale connectivity values
#'
#' @return
#' @export
#'
#'
connectivity_ <- function(cor_list, k = 20, scale = T) {

  # Add total and intracluster connectivity
  for (k in 1:k) {

    for (l in 1:k) {

      cor_list[["connectivity"]][[paste(k, l, sep = ".")]] <- cor_list %>%
        get_cor_data(name = "adjacency", k = k, l = l) %>%
        connectivity(scale = T)

    }

  }

  # Return list
  return(cor_list)

}
