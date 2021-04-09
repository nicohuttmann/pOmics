#' Computes a correlation matrix
#'
#' @param variables variables (see get variables)
#' @param observations observations (see get observations)
#' @param observations.set set of observations
#' @param data.name data name
#' @param dataset dataset
#' @param similarity.method similarity function
#' @param adjacency.method adjacency function
#' @param alpha alpha parameter for sigmoid function
#' @param theta theta parameter for sigmoid function
#' @param beta beta parameter for power function
#' @param clustering.method method for function hclust
#'
#' @return
#' @export
#'
#'
cor_matrix <- function(variables, observations, observations.set, data.name, dataset,
                       similarity.method = "none", adjacency.method = "none", alpha, theta, beta,
                       clustering.method = "average") {

  #
  cor.list <- tibble::lst()

  # Get data
  cor.list[["data"]] <- get_data(variables = !!dplyr::enquo(variables),
                                   observations = !!dplyr::enquo(observations),
                                   observations.set = observations.set,
                                   data.name = data.name,
                                   dataset = dataset)

  # Test data
  if (anyNA(cor.list[["data"]])) stop("NAs not allowd yet.")



  # Calculate Pearson correlation coefficient
  cor.list[["matrix"]] <- cor(cor.list[["data"]], method = "pearson")


  # Similarity
  cor.list[["matrix"]] <- similarity(cor.list[["matrix"]],
                                     method = similarity.method)

  # Adjacency

  cor.list[["matrix"]] <- adjacency(cor.list[["matrix"]],
                                    method = adjacency.method,
                                    alpha = alpha, theta = theta, beta = beta)


  # Calculate distance
  dist <- dist(x = cor.list[["matrix"]])


  # Compute clustering dendrogram
  cor.list[["dendrogram"]] <- fastcluster::hclust(d = dist, method = clustering.method)

  # Return
  return(cor.list)

}
