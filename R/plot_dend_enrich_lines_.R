#' Plots -log10 p-value profile of dendrogram enrichment from cor_list object
#'
#' @param cor_list cor_list object
#' @param name name of enrichment
#'
#' @return
#' @export
#'
#'
plot_dend_enrich_lines_ <- function(cor_list, name) {
  
  # given name
  if (hasArg(name) && paste0("dend.enrich_", name) %in% names(cor_list)) {
    plot_dend_enrich_lines(dend.table.pvalue = cor_list[[paste0("dend.enrich_", name)]][["dend.table.pvalue"]],
                           dend.table.cluster = cor_list[[paste0("dend.enrich_", name)]][["dend.table.cluster"]])
    # No name given
  } else if (any(grepl("dend.enrich", names(cor_list)))) {
    # Only one dend.enrich list in cor_object
    if (sum(grepl("dend.enrich", names(cor_list))) == 1) {
      plot_dend_enrich_lines(dend.table.pvalue = cor_list[[names(cor_list)[grep("dend.enrich", names(cor_list))]]][["dend.table.pvalue"]],
                             dend.table.cluster = cor_list[[names(cor_list)[grep("dend.enrich", names(cor_list))]]][["dend.table.cluster"]])
    # More than one dend.enrich list in cor_object
    } else {
      message(paste0("Using ", names(cor_list)[first_(grep("dend.enrich", names(cor_list)))], " for plot."))
      plot_dend_enrich_lines(dend.table.pvalue = cor_list[[names(cor_list)[first_(grep("dend.enrich", names(cor_list)))]]][["dend.table.pvalue"]],
                             dend.table.cluster = cor_list[[names(cor_list)[first_(grep("dend.enrich", names(cor_list)))]]][["dend.table.cluster"]])
    }
  # No dend.enrich list object
  } else {
    stop("No enrichment list in cor_object.")
  }
  
}
