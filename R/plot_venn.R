#' Plots a venn diagram form given data
#'
#' @param x table or list
#' @param name name
#' @param plot plot venn diagrams to device
#' @param export exports plot to pdf

#'
#' @return
#' @export
#'
#' @importFrom rlang sym
#' @import ggplot2
#' @import dplyr
#' @import ggvenn
#'
plot_venn <- function(x, name, plot = T, export = F) {

  #
  if (!hasArg(x)) {
    stop("No data given.")
    # Data frame or tibble given
  } else if (is.data.frame(x)) {
    venn <- x

  } else if (is.list(x)) {

    venn <- tibble::tibble(variables = unique(unlist(x)))

    # Add logical columns
    for (column in names(x)) {
      venn <- venn %>%
        mutate(!!column := variables %in% x[[column]])
    }

  }




  if (ncol(venn) == 3) {
    p <- ggplot2::ggplot(venn) +
    ggvenn::geom_venn(mapping = aes(A = !!rlang::sym(colnames(venn)[2]), B = !!rlang::sym(colnames(venn)[3])),
                      digits = 1,
                      stroke_size = 1 / .pt * 4 / 3,
                      text_size = 8 / .pt,
                      set_name_size = 8 / .pt,
                      fill_color = "white") +
      coord_fixed() +
      theme_void()
  } else if (ncol(venn) == 4) {
    p <- ggplot2::ggplot(venn) +
    ggvenn::geom_venn(mapping = ggplot2::aes(A = !!rlang::sym(colnames(venn)[2]), B = !!rlang::sym(colnames(venn)[3]), C = !!rlang::sym(colnames(venn)[4])),
                      digits = 1,
                      stroke_size = 1 / .pt * 4 / 3,
                      text_size = 8 / .pt,
                      set_name_size = 8 / .pt,
                      fill_color = "white") +
      coord_fixed() +
      theme_void()
  }





  print(p)
  # Plot
  #pdf(ifelse(hasArg(name), paste0(name, ".pdf"), "test.pdf"), width = 4, height = 4)
  # Print on canvas
  #print(p)
  #
  #dev.off()






}
