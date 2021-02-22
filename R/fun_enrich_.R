#' Functional enrichment of cor_list object
#'
#' @description Either name or k must be given. Number of clusters will be analysed as groups if no l is specified.
#'
#' @param cor_list cor_list object
#' @param name (optional) name of enrichment entry
#' @param k (optional) number of clusters
#' @param l (optional) number of cluster to analyse specifically
#' @param databases databases for which to perform enrichment
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#'
#' @return
#' @export
#'
#'
fun_enrich_ <- function(cor_list, name, k, l, databases = "GO", algorithm = "weight01") {

  # if enrichment name given
  if (hasArg(name)) {

    if (!paste0("dend.enrich_", name) %in% names(cor_list)) stop("Name not found.")

    else fun_enrich(proteins = get_cluster_cut(cor_list = cor_list,
                                               k = cor_list[[paste0("dend.enrich_", name)]][["k"]]) ==
                      cor_list[[paste0("dend.enrich_", name)]][["l"]],
                    databases = databases,
                    algorithm = algorithm,
                    view = TRUE,
                    save = FALSE)

  } else if (hasArg(k)) {

    if (hasArg(l)) {

      fun_enrich(proteins = get_cluster_cut(cor_list = cor_list, k = k) == l,
                 databases = databases,
                 algorithm = algorithm,
                 view = TRUE,
                 save = FALSE)

    } else {

      proteins <- get_cluster_cut(cor_list = cor_list, k = k)
      mode(proteins) <- "character"
      fun_enrich(proteins = proteins,
                 databases = databases,
                 algorithm = algorithm,
                 view = TRUE,
                 save = FALSE)

    }

  }

}
