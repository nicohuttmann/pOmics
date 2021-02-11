#' TPlots lines and cormat enrichment
#'
#' @param cor_list cor_list_object
#' @param name nem of enrichment
#' @param labels proteins to label yaxis
#'
#' @return
#' @export
#'
#'
plot_enrich <- function(cor_list, name, labels) {
  
  # Plot lines
  plot_dend_enrich_lines_(cor_list = cor_list, name = name)
  
  # Plot matrix
  if (!hasArg(labels)) {
    plot_cormat_enrichment(cor_list = cor_list, name = name)
  } else {
    plot_cormat_enrichment_labels(cor_list = cor_list, name = name, labels = labels)
  }
  
}
