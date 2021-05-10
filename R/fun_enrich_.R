#' Functional enrichment of cor_list object
#'
#' @description Either name or k must be given. Clusters will be analysed as groups if no l is specified.
#'
#' @param cor_list cor_list object
#' @param name (optional) name of enrichment entry
#' @param k (optional) number of clusters
#' @param l (optional) number of cluster to analyse specifically
#' @param inverse scores: enriches for higher scores if TRUE
#' @param database databases for which to perform enrichment
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param add.info Add additional information (takes longer)
#' @param view View enrichment tables
#'
#' @return
#' @export
#'
#'
fun_enrich_ <- function(cor_list, name, k, l, inverse = F, database = "GO", algorithm = "weight01", add.info = T, view = T) {

  # if enrichment name given
  if (hasArg(name)) {

    if (!paste0("dend.enrich_", name) %in% names(cor_list)) stop("Name not found.")

    else {

      results <- fun_enrich(proteins = get_cluster_cut(cor_list = cor_list,
                                            k = cor_list[[paste0("dend.enrich_", name)]][["k"]]) ==
                   cor_list[[paste0("dend.enrich_", name)]][["l"]],
                   inverse = inverse,
                   database = database,
                   algorithm = algorithm,
                   add.info = add.info,
                   view = view)

    }

  } else if (hasArg(k)) {

    if (hasArg(l)) {

      name <- paste0("cluster_", k, l)

      results <- fun_enrich(proteins = get_cluster_cut(cor_list = cor_list, k = k) == l,
                            inverse = inverse,
                            database = database,
                            algorithm = algorithm,
                            add.info = add.info,
                            view = view)

    } else {

      proteins <- get_cluster_cut(cor_list = cor_list, k = k)

      name <- paste0("clusters_", k)

      mode(proteins) <- "character"

      results <- fun_enrich(proteins = proteins,
                            inverse = inverse,
                            database = database,
                            algorithm = algorithm,
                            add.info = add.info,
                            view = view)

    }

  }

  # Add results to list
  cor_list[[paste0("fun_enrich_", name)]] <- results

  # Return results
  invisible(cor_list)

}
