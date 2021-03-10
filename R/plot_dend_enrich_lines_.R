#' Plots -log10 p-value profile of dendrogram enrichment from cor_list object
#'
#' @param cor_list cor_list object
#' @param name name of enrichment
#' @param print print plot to device
#'
#' @return
#' @export
#'
#'
plot_dend_enrich_lines_ <- function(cor_list, name, print = T) {

  # given name
  if (hasArg(name) && paste0("dend.enrich_", name) %in% names(cor_list)) {
    p <- plot_dend_enrich_lines(dend.table.pvalue = cor_list[[paste0("dend.enrich_", name)]][["dend.table.pvalue"]],
                                dend.table.cluster = cor_list[[paste0("dend.enrich_", name)]][["dend.table.cluster"]],
                                title = name,
                                print = print)
    # No name given
  } else if (!hasArg(name) && any(grepl("dend.enrich", names(cor_list)))) {
    # Only one dend.enrich list in cor_object
    if (sum(grepl("dend.enrich", names(cor_list))) == 1) {
      p <- plot_dend_enrich_lines(dend.table.pvalue = cor_list[[names(cor_list)[grep("dend.enrich", names(cor_list))]]][["dend.table.pvalue"]],
                                  dend.table.cluster = cor_list[[names(cor_list)[grep("dend.enrich", names(cor_list))]]][["dend.table.cluster"]],
                                  title = name,
                                  print = print)
    # More than one dend.enrich list in cor_object
    } else if (!hasArg(name)) {
      message(paste0("Using ", names(cor_list)[first_element(grep("dend.enrich", names(cor_list)))], " for plot."))
      p <- plot_dend_enrich_lines(dend.table.pvalue = cor_list[[names(cor_list)[first_element(grep("dend.enrich", names(cor_list)))]]][["dend.table.pvalue"]],
                                  dend.table.cluster = cor_list[[names(cor_list)[first_element(grep("dend.enrich", names(cor_list)))]]][["dend.table.cluster"]],
                                  title = name,
                                  print = print)
    }
  # No dend.enrich list object
  } else {
    stop("No enrichment list in cor_object.")
  }


  cor_list[[paste0("plot_lines_", name)]] <- p


  # Return
  invisible(cor_list)

}
