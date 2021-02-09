#' Calculates enrichment for each cluster level
#'
#' @param cor_list cor_list object
#' @param proteins annotated proteins or protein score vector
#' @param n max number of clusters
#' @param inverse inverse enrichment (works for both Fisher and KS test)
#' @param name name
#' @param plot Plot results?
#' @param cut cut dendrogram
#' @param return return cor_list object
#' @param fun.enrich Do functional enrichment
#' @param labels proteins to label on y-axis
#' @param databases databases to use for functional enrichment
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
enrich_dend_ <- function(cor_list, proteins, n = 10, inverse = F, name = "test", cut = T, return = T, plot = T, labels, fun.enrich = T, databases = "GO", algorithm = "weight01") {

  # Check input
  if(!hasArg(cor_list) || !hasArg(proteins)) stop("Missing input.")

  # name
  name <- ask_name(name = name, message = "Name for enrichment type: ")

  # Test anotation input; if numeric -> ks test
  if (is.numeric(proteins) && length(names(proteins)) > 0) {
    cor_list[[paste0("dend.enrich_", name)]] <- enrich_dend_ks(dend.table = cor_list[["dend.table"]],
                                                               protein.scores = proteins,
                                                               n = n,
                                                               inverse = inverse) %>%
                                                eval_dend_enrich(cut = cut)
  # Test anotation input; if protein vector -> fisher test
  } else if(is.character(proteins)) {
    cor_list[[paste0("dend.enrich_", name)]] <- enrich_dend_fisher(dend.table = cor_list[["dend.table"]],
                                                                   annotated.proteins = proteins,
                                                                   n = n,
                                                                   inverse = inverse) %>%
                                                eval_dend_enrich(cut = cut)
  # Incorrect input
  } else {
    stop("Incorrect input.")
  }





  # Plot enrichment results
  if(plot) {
    plot_dend_enrich_lines_(cor_list = cor_list, name = name)
    # Plot lines
    # plot cormat
    if (!hasArg(labels)) {
      plot_cormat_enrichment(cor_list = cor_list, name = name)
    } else {
      plot_cormat_enrichment_labels(cor_list = cor_list, name = name, labels = labels)
    }
  }

  # Functional enrichment
  if (fun.enrich) fun_enrich_(cor_list = cor_list, name = name, databases = databases, algorithm = algorithm)

  # Return
  if (return) return(cor_list)

}
