#' Plots barplot as used by clusterProfiler
#'
#' @param data enrichResult
#' @param x one of 'Count' and 'GeneRatio'
#' @param color one of 'pvalue', 'p.adjust' and 'qvalue'
#' @param showCategory number of categories to show
#' @param font.size font size
#' @param title plot title
#' @param label_format a numeric value sets wrap length, alternatively a custom function to format axis labels. by default wraps names longer that 30 characters
#' @param silent suppress error message and only return NULL
#'
#' @return
#' @export
#'
#'
plot_enrichment_barplot <- function(data, x = "Count", color = "p.adjust", showCategory = 8, font.size = 10,
                                    title = "", label_format = 20, inverse = F, silent = F) {

  # Test input
  if (class(data) != "enrichResult") {

    if (!silent) message("No enrichResult object given.")

    return(invisible(NULL))

  } else if (nrow(as.data.frame(data)) == 0) {

    if (!silent) message("No significant annotations.")

    return(invisible(NULL))

  }

  #
  p <- barplot(data,
               x = x,
               color = color,
               showCategory = showCategory,
               font.size = font.size,
               title = title,
               label_format = label_format) +
    theme_hjv_half_open(font_size = font.size)

  if (inverse) p <- p + scale_x_reverse() + scale_y_discrete(position = "right") + theme(legend.position = "left")

  # Return
  return(p)

}
